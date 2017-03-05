{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module InfoVis.Parallel.Process.Select (
  selecter
) where


import Control.Arrow (second)
import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.Distributed.Process (Process, ProcessId, expect, getSelfPid, liftIO, say, send)
import Control.Monad (forever, void)
import Data.Function.MapReduce (mapReduce)
import Graphics.Rendering.OpenGL (Vertex3(..))
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import InfoVis.Parallel.Process.DataProvider (GridsLinks)
import InfoVis.Parallel.Process.Util (collectMessages)
import InfoVis.Parallel.Types (Coloring(..))
import InfoVis.Parallel.Types.Configuration (Configuration(..))
import InfoVis.Parallel.Types.Display (DisplayList(..))
import InfoVis.Parallel.Types.Message (DisplayerMessage(..), SelecterMessage(..), SelectionAction(..))
import InfoVis.Parallel.Types.Scaffold (Presentation(..), World(..))
import Linear.Affine (Point(..), (.-.))
import Linear.Conjugate (conjugate)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.Vector (zero)

import qualified Linear.Quaternion as Q (rotate)


selecter :: [ProcessId] -> Process ()
selecter listeners =
  do
    pid <- getSelfPid
    say $ "Starting selector <" ++ show pid ++ ">."
    ResetSelecter configuration <- expect
    AugmentSelecter gridsLinks@(_, links) <- expect
    relocationVar <- liftIO $ newMVar (zero, Quaternion 1 zero)
    persistentColoringsRef <- liftIO . newMVar . mapReduce id (curry $ second maximum) $ (, NormalColoring) <$> concatMap listVertexIdentifiers links
    transientColoringsRef  <- liftIO . newMVar . mapReduce id (curry $ second maximum) $ (, NormalColoring) <$> concatMap listVertexIdentifiers links
    forever
      $ do
        let
          collector   RelocateSelecter{} _ = True
          collector x@UpdateSelecter{}   y = selecterState x == selecterState y
          collector _                    _ = False
        messages <- collectMessages collector
        sequence_
          [
            case message of
              RelocateSelecter{..} -> do
                                        void . liftIO $ swapMVar relocationVar (relocationDisplacement, relocationRotation)
                                        mapM_ (`send` Relocate relocationDisplacement relocationRotation) listeners
              UpdateSelecter{..}   -> do
                                        (relocation, reorientation) <- liftIO $ readMVar relocationVar
                                        persistentColorings <- liftIO $ readMVar persistentColoringsRef
                                        transientColorings <- liftIO $ readMVar transientColoringsRef
                                        let
                                          ((persistentColorings', transientColorings'), changes) =
                                            selecter'
                                              configuration
                                              gridsLinks
                                              (persistentColorings, transientColorings)
                                              (selecterPosition, selecterState)
                                              (P relocation, reorientation)
                                        void .liftIO $ swapMVar persistentColoringsRef persistentColorings'
                                        void .liftIO $ swapMVar transientColoringsRef transientColorings'
                                        mapM_ (`send` Select selecterPosition changes) listeners
              _                    -> return ()
          |
            message <- messages
          ]


selecter' :: Configuration Double
         -> GridsLinks
         -> ([(Int, Coloring)], [(Int, Coloring)])
         -> (Point V3 Double, SelectionAction)
         -> (Point V3 Double, Quaternion Double)
         -> (([(Int, Coloring)], [(Int, Coloring)]), [(Int, Coloring)])
selecter' Configuration{..} (_, links) (persistentColorings, transientColorings) (P location, press) (P location', orientation') =
  let
    V3 x y z = realToFrac <$> (P $ conjugate orientation' `Q.rotate` location) .-. P location'
    d = realToFrac $ selectorSize presentation * baseSize world / 2
    h w w' = abs (w - w' + d) <= d
    f (Vertex3 x' y' z') = h x x' && h y y' && h z z'
    g DisplayList{..} = map (second f) $ zip listVertexIdentifiers listVertices
    selections = mapReduce id (curry $ second or) $ concatMap g links :: [(Int, Bool)]
    persistentColorings' =
      case press of
        Highlight       -> persistentColorings
        Deselection     -> zipWith (\(i, o) (_, n) -> (i, if n then NormalColoring    else o)) persistentColorings selections
        Selection       -> zipWith (\(i, o) (_, n) -> (i, if n then SelectColoring    else o)) persistentColorings selections
        Clear           -> map (\(i, _) -> (i, NormalColoring))                                persistentColorings
    transientColorings'  = zipWith (\(i, o) (_, n) -> (i, if n then HighlightColoring else o)) persistentColorings selections
  in
    (
      (persistentColorings', transientColorings')
    , fmap snd $ filter (uncurry (/=)) $ zip transientColorings transientColorings'
    )
