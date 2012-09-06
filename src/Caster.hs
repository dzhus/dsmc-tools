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
import DSMC.Traceables.Parser
import DSMC.Util.Vector


-- | Observation point.
data Camera = Camera { position :: Point
                     -- ^ Absolute camera position
                     , direction :: Vec3
                     -- ^ View direction
                     }

-- | Static options for caster.
data Options = Options
    { bodyDef :: FilePath
    , width :: Int
    , height :: Int
    , pixels :: Int
    }
    deriving (Data, Typeable)

type Ray = Particle

origin = (0, 0, 0)

-- | Pixels in meter.
scaleFactor :: Double
scaleFactor = 50.0

-- | Camera position and direction.
cam = Camera (10, 0, 0) ((-1), 0, (1))

-- | Physical distance from camera to origin.
dist = norm $ origin <-> (position cam)

casterField :: Int
            -- ^ Window width.
            -> Int
            -- ^ Window height.
            -> Int
            -- ^ Pixels per point.
            -> Body
            -- ^ Body to show.
            -> IO ()
casterField width height pixels body =
    let
        !wScale = -(fromIntegral (width `div` 2) / scaleFactor)
        !hScale = -(fromIntegral (height `div` 2) / scaleFactor)
        display = InWindow "dsmc-tools CSG raycaster" (width, height) (0, 0)
        makePixel :: Float -> G.Point -> Color
        makePixel !t' !(x, y) =
            let
                t = float2Double t'
                -- Rotate the view plane around origin as the world
                -- evolves
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
    in
      animateField display (pixels, pixels) makePixel


-- | Read body def and program arguments, run the actual caster on
-- success.
main =
    let
        sample = Options
                 { bodyDef = def &= argPos 0 &= typFile
                 , width = 500 &= help "Window width"
                 , height = 500 &= help "Window height"
                 , pixels = 1 &= help "Number of pixels to draw per point, in each dimension"
                 }
                 &= program "dsmc-caster"
    in do
      Options{..} <- cmdArgs $ sample
      body <- parseBodyFile bodyDef
      case body of
        Right b -> casterField width height pixels b
        Left e -> error $ "Problem when reading body definition: " ++ e
