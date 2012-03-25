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
import Graphics.Gloss.Field

import System.Console.CmdArgs.Implicit

import DSMC.Traceables
import DSMC.Types
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



-- | Pixels in meter.
scaleFactor = 200

main = 
    let
        body = intersection [plane (Vector 1 0 1) 0,
                             union [sphere (Vector 0 0 0.2) 0.5, 
                                    cylinder (Vector 0 1 0) (Vector 0 0 0) 0.2]]
        (w, h) = (600, 600)
        display = InWindow "dsmc-tools CSG raycaster" (w, h) (100, 100)
        p = Vector 10 0 2
        d = Vector (-10) 0 (-2)
        zoom = 1
        viewScale = zoom / scaleFactor
        !wScale = fromIntegral (w `div` 2) * viewScale
        !hScale = fromIntegral (h `div` 2) * viewScale
        camera = Camera p d
        (n, sX, sY) = buildCartesian d
        h' = fromIntegral h

        {-# INLINE makeRay #-}
        makeRay :: G.Point -> Ray
        makeRay (x, y) = 
            Particle (p 
                      <+> (sX .^ ((float2Double x) * wScale))
                      <+> (sY .^ ((float2Double y) * hScale))) n

        {-# INLINE makePixel #-}
        makePixel :: G.Point -> Color
        makePixel (x, y) =
            let
                ray = makeRay (x, y)
                fullTrace = trace body ray
                hitTrace = intersectTraces fullTrace 
                           [((infinityN, Nothing), (infinityP, Nothing))]
                n = fromJust $ snd $ fst $ head hitTrace
                factor = double2Float $ (reverse n) .* (velocity ray)
            in
              if null hitTrace then white else mixColors factor (1 - factor) red black
                  where
                      
    in
      animateField display (1, 1) (\f point -> makePixel point)


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
--         Nothing -> error "Lol u suck mon"

-- -- | Calculate color of ray.
-- rayCast :: Ray -> Body -> Color
-- rayCast ray b =
--     let
--         fullTrace = trace b ray
--         hitTrace = intersectTraces fullTrace [((0, Nothing),
--                                                (infinityP, Nothing))]
--     in
--       if null hitTrace
--       then white
--       else
--           let
--               n = fromJust (snd (fst (head hitTrace)))
--           in
--             scaleColor red ((reverse n) .* (velocity ray))
