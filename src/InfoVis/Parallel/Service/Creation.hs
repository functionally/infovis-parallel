{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}


module InfoVis.Parallel.Service.Creation (
  withVulkan
) where


import Control.Monad.Log (logDebug)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Graphics.Vulkan (_VK_MAKE_VERSION)
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import InfoVis.Parallel.Service (MonadService, checkVulkan, guardBracket, guardIO, throwService, tryVulkan)

import qualified Graphics.UI.GLFW as GLFW


withVulkan :: MonadService m
           => String
           -> Int
           -> Int
           -> (GLFW.Window -> VkInstance -> m a)
           -> m a
withVulkan title width height action =
  guardBracket
    (
      do
        initialized <- guardIO GLFW.init
        if initialized
          then logDebug "GLFW initialized."
          else throwService "GLFW initialization failed."
    )
    (
      const
        $ guardIO GLFW.terminate
        >> logDebug "GLFW terminated."
    )
    . const
    $ do
      version <- guardIO GLFW.getVersionString
      logDebug $ maybe "No GLFW version." ("GLFW version: " ++) version
      guardIO
        $ mapM_ GLFW.windowHint
        [
          GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
        , GLFW.WindowHint'Resizable False
        ]
      guardBracket
        (
          guardIO $ GLFW.createWindow width height title Nothing Nothing
        )
        (
          \window -> do
            mapM_ (guardIO . GLFW.destroyWindow) window
            logDebug "GLFW window destroyed."
        )
        $ \window ->
          case window of
            Nothing      -> throwService "GLFW window creation failed."
            Just window' -> do
                              logDebug "GLFW window created."
                              guardBracket
                                (
                                  do
                                    requirements <- guardIO GLFW.getRequiredInstanceExtensions
                                    let
                                      layers = ["VK_LAYER_LUNARG_standard_validation"]
                                      appInfo =
                                        createVk @VkApplicationInfo
                                          $  set       @"sType"              VK_STRUCTURE_TYPE_APPLICATION_INFO
                                          &* set       @"pNext"              VK_NULL
                                          &* setStrRef @"pApplicationName"   title
                                          &* set       @"applicationVersion" (_VK_MAKE_VERSION 1 0 0)
                                          &* setStrRef @"pEngineName"        "InfoVis Parallel Engine"
                                          &* set       @"engineVersion"      (_VK_MAKE_VERSION 1 0 0)
                                          &* set       @"apiVersion"         (_VK_MAKE_VERSION 1 0 68)
                                      createInfo =
                                        createVk @VkInstanceCreateInfo
                                          $  set           @"sType"                   VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
                                          &* set           @"pNext"                   VK_NULL
                                          &* setVkRef      @"pApplicationInfo"        appInfo
                                          &* set           @"enabledLayerCount"       (fromIntegral $ length layers)
                                          &* setStrListRef @"ppEnabledLayerNames"     layers
                                          &* set           @"enabledExtensionCount"   (fromIntegral $ length requirements)
                                          &* setListRef    @"ppEnabledExtensionNames" requirements
                                    checkVulkan
                                      . withPtr createInfo
                                      $ \ptr ->
                                        alloca
                                          $ \ptr' ->
                                            tryVulkan
                                              "vkCreateInstance: Failed to create vkInstance."
                                              (vkCreateInstance ptr VK_NULL ptr')
                                              (peek ptr')
                                )
                                (guardIO . flip vkDestroyInstance VK_NULL)
                                $ action window'
