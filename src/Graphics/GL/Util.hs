module Graphics.GL.Util (
  joinSwapGroup
, unsafeGetCurrentDisplay
, unsafeGetWindow
, unsafeJoinSwapGroupNV
) where


import Foreign.C.Types
import Foreign.Ptr
import Graphics.GL.GetProcAddress (getProcAddress)
import Graphics.GL.Types (GLuint)
import Graphics.UI.GLUT.Window (Window)
import Graphics.X11.Xlib.Types (Display(..))
import System.IO.Unsafe (unsafePerformIO)


joinSwapGroup :: Window -> Int -> IO Bool
joinSwapGroup win grp =
  if ptr_glxGetCurrentDisplay == nullFunPtr || ptr_glxJoinSwapGroupNV == nullFunPtr
    then return False
    else do
           dpy <- unsafeGetCurrentDisplay
           let
             win' = getWindow win
             grp' = fromIntegral grp
           unsafeJoinSwapGroupNV dpy win' grp'

    
unsafeGetCurrentDisplay :: IO Display
unsafeGetCurrentDisplay = dyn_glxGetCurrentDisplay ptr_glxGetCurrentDisplay

foreign import ccall "dynamic" dyn_glxGetCurrentDisplay :: FunPtr (IO Display) -> IO Display

{-# NOINLINE ptr_glxGetCurrentDisplay #-}
ptr_glxGetCurrentDisplay :: FunPtr a
ptr_glxGetCurrentDisplay = unsafePerformIO $ getProcAddress "glXGetCurrentDisplay"


getWindow :: Window -> CInt
getWindow = read . drop 7 . show -- FIXME: Depends on implementation of 'Window.show.


unsafeGetWindow :: IO CInt
unsafeGetWindow = dyn_glutGetWindow ptr_glutGetWindow

foreign import ccall "dynamic" dyn_glutGetWindow :: FunPtr (IO CInt) -> (IO CInt)

{-# NOINLINE ptr_glutGetWindow #-}
ptr_glutGetWindow :: FunPtr a
ptr_glutGetWindow = unsafePerformIO $ getProcAddress "glutGetWindow"


unsafeJoinSwapGroupNV :: Display -> CInt -> GLuint -> IO Bool
unsafeJoinSwapGroupNV = dyn_glxJoinSwapGroupNV ptr_glxJoinSwapGroupNV

foreign import ccall "dynamic" dyn_glxJoinSwapGroupNV :: FunPtr (Display -> CInt -> GLuint -> IO Bool) -> (Display -> CInt -> GLuint -> IO Bool)

{-# NOINLINE ptr_glxJoinSwapGroupNV #-}
ptr_glxJoinSwapGroupNV :: FunPtr a
ptr_glxJoinSwapGroupNV = unsafePerformIO $ getProcAddress "glXJoinSwapGroupNV"
