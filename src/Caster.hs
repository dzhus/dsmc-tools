{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

{-|
  
Standalone raycasting runner for traceable objects.
  
|-}

import Prelude hiding (reverse)

import Data.Maybe

import GHC.Float

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import qualified Graphics.Gloss.Data.Point as G
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Graphics.Gloss.Raster.Field hiding (Point)

import System.Console.CmdArgs.Implicit

import qualified Data.Strict.Maybe as S
import qualified Data.Strict.Tuple as S

import DSMC.Traceables
import DSMC.Types hiding (position)
import DSMC.Util.Vector


type Ray = Particle

-- | Observation point.
data Camera = Camera { position :: Point
                     -- ^ Absolute camera position
                     , direction :: Vector
                     -- ^ View direction
                     }


data Options = Options
    { bodyDef :: Maybe FilePath }
    deriving (Data, Typeable)

data World = World
    { camera :: Camera
    }


-- | Pixels in meter.
scaleFactor = 100

origin = Vector 0 0 0

processEvents :: G.Event -> World -> World
processEvents event world =
    case event of
      G.EventKey (G.SpecialKey k) G.Down _ _ ->
          let
              cam = camera world
              (pos, dir) = (position cam, direction cam)
              (n, sX, sY) = buildCartesian dir
              newPos = 
                  case k of
                    G.KeyLeft -> pos <+> sX
                    G.KeyRight -> pos <-> sX
                    G.KeyUp -> pos <+> sY
                    G.KeyDown -> pos <-> sY
                    _ -> pos
          in
            world{camera = cam{position = newPos,
                               direction = origin <-> newPos}}
      G.EventKey _ _ _ _ -> world
      G.EventMotion _ -> world

main = 
    let

        body = Intersection (Intersection (Intersection (Intersection
               (Cylinder (Vector 1 0 0) (Vector 0 0 0) 3)
               (Cylinder (Vector 0 1 0) (Vector 0 0 0) 3))
               (Cone (Vector 0 0 1) (Vector 0 0 0) (pi / 6)))
               (Cylinder (Vector 0 0 1) (Vector 0.3 0.2 0) 1))
               (Cylinder (Vector 0.1 0.05 1) (Vector (-0.3) 0.2 0) 1)
               

        (width, height) = (1600, 1000)
        display = InWindow "dsmc-tools CSG raycaster" (width, height) (100, 100)

        zoom = 1.0
        viewScale = zoom / scaleFactor
        !wScale = fromIntegral (width `div` 2) * viewScale
        !hScale = fromIntegral (height `div` 2) * viewScale

        world = World $ Camera (Vector 10 0 10) (Vector (-1) 0 (-1))

        makePixel :: World -> G.Point -> Color
        makePixel !w !(x, y) =
            let
                (n, sX, sY) = buildCartesian $ direction $ camera w
                p = Main.position $ camera w
                ray :: Ray
                ray = Particle (p 
                                <+> (sX .^ ((float2Double x) * wScale))
                                <+> (sY .^ ((float2Double y) * hScale))) n

                hp = hitPoint body ray
            in
              case hp of
                S.Just (HitPoint _ (S.Just n)) -> 
                    mixColors factor (1 - factor) red blue
                    where
                      factor = double2Float $ reverse n .* velocity ray
                _ -> white
        {-# INLINE makePixel #-}
    in
      playField display (1, 1) 1
                world 
                makePixel
                processEvents
                (\_ w -> w)


-- main :: IO ()
-- main = 
--     let
--         sample = Options
--                  { bodyDef = def
--                    &= help "Path to body definition file"
--                  }
--     in do
--       Options{..} <- cmdArgs $ sample
--       case bodyDef of
--         Just fname -> print fname
--         Nothing -> error "No body definition given"
