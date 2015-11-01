module Graphics.UI.Handa.Util (
  DlpViewerDisplayCallback
, dlpViewerDisplay
) where


import Data.IORef (IORef)
import Graphics.Rendering.DLP (DlpState, drawDlp)
import Graphics.Rendering.Handa.Viewer (ViewerParameters, loadViewer)
import Graphics.Rendering.OpenGL (flush)
import Graphics.UI.GLUT (DisplayCallback, swapBuffers)


type DlpViewerDisplayCallback = Maybe (IORef DlpState) -> ViewerParameters -> DisplayCallback


dlpViewerDisplay :: DisplayCallback -> DlpViewerDisplayCallback
dlpViewerDisplay display dlp viewerParameters =
  do
    loadViewer dlp viewerParameters
    display
    maybe (return ()) drawDlp dlp
    flush
    swapBuffers
