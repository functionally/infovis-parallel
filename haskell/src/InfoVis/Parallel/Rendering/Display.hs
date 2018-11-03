{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module InfoVis.Parallel.Rendering.Display (
  Changes(..)
, displayer
) where


import Control.Arrow ((&&&))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, isEmptyTMVar, putTMVar)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVar)
import Control.DeepSeq (NFData)
import Control.Exception (IOException, SomeException, handle, throw)
import Control.Monad (join, unless, void, when)
import Data.Binary (Binary)
import Data.Default (Default(..))
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import GHC.Generics (Generic)
import Graphics.OpenGL.Util.Setup (dlpViewerDisplay, idle, setup)
import Graphics.Rendering.OpenGL (GLfloat, ($=))
import Graphics.Rendering.OpenGL.GL.CoordTrans (preservingMatrix, scale, translate)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3(..))
import Graphics.Rendering.OpenGL.GL.VertexSpec (color)
import Graphics.UI.GLUT.Begin (mainLoop)
import Graphics.UI.GLUT.Callbacks.Global (idleCallback)
import Graphics.UI.GLUT.Fonts (StrokeFont(Roman), fontHeight, renderString, stringWidth)
import Graphics.UI.GLUT.Objects (Flavour(Solid), Object(Sphere'), renderObject)
import InfoVis.Parallel.Process.Util (debugTimeIO, makeDebuggerIO, makeTimer)
import InfoVis.Parallel.Rendering.Shapes (DisplayBuffer(bufferIdentifier), drawBuffer, makeBuffer, updateBuffer)
import InfoVis.Parallel.Rendering.Types (DisplayList(..), DisplayText(..), DisplayType(..))
import InfoVis.Parallel.Types (Coloring, Location)
import InfoVis.Parallel.Types.Configuration (AdvancedSettings(..), Configuration(..))
import InfoVis.Parallel.Types.Presentation (Presentation(..))
import InfoVis.Parallel.Types.World (World(..), WorldExtent(..))
import Linear.Affine (Point(..), (.-.))
import Linear.Quaternion (Quaternion(..))
import Linear.Util (rotationFromPlane)
import Linear.Util.Graphics (toRotation, toVector3)
import Linear.V3 (V3(..))
import Linear.Vector (zero)
import System.IO (hPutStrLn, stderr)

#ifdef INFOVIS_SWAP_GROUP
import Graphics.OpenGL.Functions (joinSwapGroup)
#endif


data Changes =
  Changes
  {
    sync              :: Bool
  , dirty             :: Bool
  , eyeLocation       :: Point V3 Double
  , eyeOrientation    :: Quaternion Double
  , centerOffset      :: V3 Double
  , centerOrientation :: Quaternion Double
  , currentTime       :: String
  , selectLocation    :: Point V3 Double
  , selectChanges     :: [((DisplayType, String), [(Int, Coloring)])]
  , newText           :: [DisplayText String Location]
  , newDisplay        :: [DisplayList (DisplayType, String) Int]
  }
    deriving (Binary, Eq, Generic, NFData, Ord, Show)

instance Default Changes where
  def =
    Changes
    {
      sync              = False
    , dirty             = True
    , eyeLocation       = zero
    , eyeOrientation    = Quaternion 1 zero
    , centerOffset      = zero
    , centerOrientation = Quaternion 1 zero
    , currentTime       = ""
    , selectLocation    = zero
    , selectChanges     = []
    , newText           = []
    , newDisplay        = []
    }


displayer :: Configuration
          -> Int
          -> TVar Changes
          -> TMVar Bool
          -> IO ()
displayer Configuration{..} displayIndex changesVar readyVar =
  do
    frameDebugIO <-
      case debugDisplay <$> advanced of
        Just True -> makeDebuggerIO advanced
        _         -> return . const . const $ return ()
    currentHalfFrameIO <- makeTimer
    let
      debugTimeIO' = debugTimeIO frameDebugIO currentHalfFrameIO
      Just AdvancedSettings{..} = advanced
    dlp <- setup debugOpenGL "InfoVis Parallel" "InfoVis Parallel" viewers displayIndex
    fontHeight' <-
       handle ((\_ -> fromIntegral <$> stringWidth Roman "wn") :: IOException -> IO GLfloat)
         $ fontHeight Roman
    drawTextVar <- newIORef $ return ()
    drawStatusVar <- newIORef . const $ return ()
    buffersVar <- newIORef []
    currentVar <- newIORef def
    let
      selector = color c >> renderObject Solid (Sphere' (s / 2) 12 8)
        where
          s = selectorSize presentation * baseSize world
          c = selectorColor presentation
      erase c = c {dirty = False, selectChanges = [], newText = [], newDisplay = []}
      changeLoop =
        do
          ready <- atomically $ isEmptyTMVar readyVar
          when ready
            $ do
              f0 <- currentHalfFrameIO
              current <-
                atomically
                  $ do
                    when synchronizeDisplays
                      . void $ putTMVar readyVar True
                    current' <- readTVar changesVar
                    modifyTVar' changesVar erase
                    return current'
              writeIORef currentVar current
              when (dirty current && useIdleLoop)
                idle
              when (dirty current)
                . void $ debugTimeIO' [show displayIndex, "CHANGE", "LOCS"] f0
          Changes{..} <- readIORef currentVar
          unless (null selectChanges)
            $ do
                f0 <- currentHalfFrameIO
                buffers <- readIORef buffersVar
                mapM_ (flip (updateBuffer selectBuffer) selectChanges) buffers
                modifyIORef' currentVar $ \c -> c {selectChanges = []}
                void $ debugTimeIO' [show displayIndex, "SELECT", "SELECT"] f0
          unless (null newText)
            $ do
              f0 <- currentHalfFrameIO
              writeIORef drawTextVar
                . mapM_ (showText Nothing)
                $ tail newText
              writeIORef drawStatusVar
                $ (`showText` head newText) . Just
              modifyIORef' currentVar $ \c -> c {newText = []}
              void $ debugTimeIO' [show displayIndex, "SET", "TEXT"] f0
          unless (null newDisplay)
            $ do
              f0 <- currentHalfFrameIO
              newBuffers <- mapM makeBuffer newDisplay
              modifyIORef' buffersVar (newBuffers ++)
              modifyIORef' currentVar $ \c -> c {newDisplay = []}
              void $ debugTimeIO' [show displayIndex, "ADD", "DISPLAY"] f0
      showText t DisplayText{..} =
        let
          WorldExtent{..} = worldExtent world
          s = realToFrac textSize * realToFrac (baseSize world) / fontHeight' :: GLfloat
          qrot = rotationFromPlane (V3 1 0 0) (V3 0 (-1) 0) textOrigin textWidth textHeight
        in
          preservingMatrix $ do
            color textColor
            translate $ toVector3 (realToFrac <$> textOrigin .-. zero :: V3 GLfloat)
            toRotation qrot
            scale s s s
            translate $ Vector3 0 (- fontHeight') 0
            renderString Roman $ maybe textContent (textContent ++) t
      selectBuffer :: (DisplayType, String) -> (DisplayType, String) -> Bool
      selectBuffer buffer (LinkType, _) = fst buffer == LinkType
      selectBuffer buffer key           = buffer     == key
      onlyCurrentTime time buffer =
        fst i == GridType || i == (LinkType, time)
          where
            i = bufferIdentifier buffer
    idleCallback $=
      Just (
        if useIdleLoop
          then reportException "idle loop" changeLoop
          else idle
      )
    dlpViewerDisplay dlp viewers displayIndex ((eyeLocation &&& eyeOrientation) <$> readIORef currentVar)
      $ reportException "display loop"
      $ do
        unless useIdleLoop changeLoop
        f0 <- currentHalfFrameIO
        preservingMatrix
          $ do
            P location <- selectLocation <$> readIORef currentVar
            translate (toVector3 $ realToFrac <$> location :: Vector3 GLfloat)
            selector
        f1 <- debugTimeIO' [show displayIndex, "DRAW", "SELECT"] f0
        preservingMatrix
          $ do
            time <- currentTime <$> readIORef currentVar
            (location, orientation) <- (centerOffset &&& centerOrientation) <$> readIORef currentVar
            toRotation orientation
            translate (toVector3 $ realToFrac <$> location :: Vector3 GLfloat)
            buffers <- readIORef buffersVar
            mapM_ drawBuffer $ filter (onlyCurrentTime time) buffers
            f2 <- debugTimeIO' [show displayIndex, "DRAW", "GEOM"] f1
            join $ readIORef drawTextVar
            join $ ($ time) <$> readIORef drawStatusVar
            void $ debugTimeIO' [show displayIndex, "DRAW", "TEXT"] f2
        void $ debugTimeIO' [show displayIndex, "DRAW", "TOTAL"] f0
#ifdef INFOVIS_SWAP_GROUP
    _ <- maybe (return False) joinSwapGroup useSwapGroup
#endif
    reportException "main loop" mainLoop


reportException :: String -> IO () -> IO ()
reportException location =
  handle $ \e ->
    do
      hPutStrLn stderr $ "Rendering exception in " ++ location ++ ": " ++ show e ++ "."
      throw (e :: SomeException)
