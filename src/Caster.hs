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
import Graphics.Gloss.Field

import System.Console.CmdArgs.Implicit

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
scaleFactor = 200

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
        body = intersection [sphere (Vector 0 0 0) 1,
                             plane (Vector 1 0 1) 0,
                             plane (Vector (-1) 0 1) 0,
                             complement $ 
                             cylinder (Vector 0 1 0) (Vector 0 0 0) 0.2]
                                    

        (width, height) = (600, 600)
        display = InWindow "dsmc-tools CSG raycaster" (width, height) (100, 100)

        zoom = 1.0
        viewScale = zoom / scaleFactor
        !wScale = fromIntegral (width `div` 2) * viewScale
        !hScale = fromIntegral (height `div` 2) * viewScale

        world = World $ Camera (Vector 10 0 2) (Vector (-10) 0 (-2))

        makePixel :: World -> G.Point -> Color
        makePixel w (x, y) =
            let
                (n, sX, sY) = buildCartesian $ direction $ camera w
                p = Main.position $ camera w
                ray :: Ray
                ray = Particle (p 
                                <+> (sX .^ ((float2Double x) * wScale))
                                <+> (sY .^ ((float2Double y) * hScale))) n

                fullTrace = trace body ray
                hitTrace = intersectTraces fullTrace 
                           [((infinityN, Nothing), (infinityP, Nothing))]
                surfaceNormal = fromJust $ snd $ fst $ head hitTrace
                factor = double2Float $ (reverse surfaceNormal) .* (velocity ray)
            in
              if null hitTrace then white else mixColors factor (1 - factor) red black
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
