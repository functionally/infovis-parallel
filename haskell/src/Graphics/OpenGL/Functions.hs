module Graphics.OpenGL.Functions (
  joinSwapGroup
, unsafeGetCurrentDisplay
, unsafeGetCurrentDrawable
, unsafeJoinSwapGroupNV
) where


import Foreign.C.Types (CInt(..))
import Foreign.Ptr (FunPtr, nullFunPtr)
import Graphics.GL.GetProcAddress (getProcAddress)
import Graphics.GL.Types (GLuint)
import Graphics.X11.Xlib.Types (Display(..))
import System.IO.Unsafe (unsafePerformIO)


{-# ANN module "HLint: ignore Use camelCase" #-}


joinSwapGroup :: Int -> IO Bool
joinSwapGroup grp =
  if nullFunPtr `elem` [ptr_glxGetCurrentDisplay, ptr_glXGetCurrentDrawable, ptr_glxJoinSwapGroupNV]
    then return False
    else do
           dpy <- unsafeGetCurrentDisplay
           win <- unsafeGetCurrentDrawable
           unsafeJoinSwapGroupNV dpy win $ fromIntegral grp

    
unsafeGetCurrentDisplay :: IO Display
unsafeGetCurrentDisplay = dyn_glxGetCurrentDisplay ptr_glxGetCurrentDisplay

foreign import ccall "dynamic" dyn_glxGetCurrentDisplay :: FunPtr (IO Display) -> IO Display

{-# NOINLINE ptr_glxGetCurrentDisplay #-}
ptr_glxGetCurrentDisplay :: FunPtr a
ptr_glxGetCurrentDisplay = unsafePerformIO $ getProcAddress "glXGetCurrentDisplay"


unsafeGetCurrentDrawable :: IO CInt
unsafeGetCurrentDrawable = dyn_glXGetCurrentDrawable ptr_glXGetCurrentDrawable

foreign import ccall "dynamic" dyn_glXGetCurrentDrawable :: FunPtr (IO CInt) -> IO CInt

{-# NOINLINE ptr_glXGetCurrentDrawable #-}
ptr_glXGetCurrentDrawable :: FunPtr a
ptr_glXGetCurrentDrawable = unsafePerformIO $ getProcAddress "glXGetCurrentDrawable"


unsafeJoinSwapGroupNV :: Display -> CInt -> GLuint -> IO Bool
unsafeJoinSwapGroupNV = dyn_glxJoinSwapGroupNV ptr_glxJoinSwapGroupNV

foreign import ccall "dynamic" dyn_glxJoinSwapGroupNV :: FunPtr (Display -> CInt -> GLuint -> IO Bool) -> (Display -> CInt -> GLuint -> IO Bool)

{-# NOINLINE ptr_glxJoinSwapGroupNV #-}
ptr_glxJoinSwapGroupNV :: FunPtr a
ptr_glxJoinSwapGroupNV = unsafePerformIO $ getProcAddress "glXJoinSwapGroupNV"
