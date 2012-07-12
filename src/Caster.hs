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

import DSMC.Particles
import DSMC.Traceables
import DSMC.Util.Vector


-- | Observation point.
data Camera = Camera { position :: Point
                     -- ^ Absolute camera position
                     , direction :: Vec3
                     -- ^ View direction
                     }


data Options = Options
    { bodyDef :: Maybe FilePath }
    deriving (Data, Typeable)

data World = World
    { camera :: Camera
    }

type Ray = Particle

-- | Pixels in meter.
scaleFactor = 150

origin = (0, 0, 0)


-- | Body definition.
body :: Body
body = intersect 
       (cone (0, 0, 1) (0, 0, 0) (pi / 12))
       (sphere (0, 5, 0) 10)


-- | Camera position and direction.
cam = Camera (10, 0, 0) ((-1), 0, (1))


-- | Window width.
width = 1000


-- | Window height.
height = 1000


-- | Zoom factor.
zoom = 40.0


-- | Physical distance from camera to origin.
dist = 10.0


main =
    let
        !wScale = -(fromIntegral (width `div` 2) / zoom)
        !hScale = -(fromIntegral (height `div` 2) / zoom)
        !p = Main.position cam
        makePixel :: Float -> G.Point -> Color
        makePixel !t' !(x, y) =
            let
                t = float2Double t'
                cx = sin t
                cy = cos t
                (n, sX, sY) = buildCartesian (cx, cy, 0)
                p = (-dist * cx, -dist * cy, 0)
                ray :: Ray
                ray = ((p
                        <+> (sX .^ ((float2Double x) * wScale))
                        <+> (sY .^ ((float2Double y) * hScale))), n)

                !hp = trace body ray
            in
              case hp of
                (((S.:!:) (HitPoint _ (S.Just hn)) _):_) ->
                    mixColors factor (1 - factor) red blue
                    where
                      factor = double2Float $ reverse n .* hn
                _ -> white
        {-# INLINE makePixel #-}
        world = World cam
        display = InWindow "dsmc-tools CSG raycaster" (width, height) (100, 100)
    in
      animateField display (1, 1) makePixel


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
