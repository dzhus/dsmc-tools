module Main

where

import Data.List (unfoldr)

import DSMC
import DSMC.Types
import DSMC.Traceables
import DSMC.Util.Vector

plotParticle :: Particle -> String
plotParticle (Particle (Vector x y z) (Vector vx vy vz)) = 
    unwords (map show [x, y, z, vx, vy, vz])

exportParticles :: [Particle] -> IO ()
exportParticles particles = do
     mapM_ (putStrLn . plotParticle) particles

main :: IO ()
main =
    let
        plane1 = (plane (Vector 0 0 1) 0)
        plane2 = (plane (Vector 0 0 (-1)) (-1))
        plane3 = (plane (Vector 0 1 1) 0)
        sphere1 = (sphere (Vector 0 0 0) 4)
        sphere2 = (sphere (Vector 0 0 (-2)) 3)
        sphere3 = (sphere (Vector 4 0 1.02) 1.2)
        body = intersection [(union [(intersection [plane1, plane2]), sphere2]), plane3]
        dt = 0.05
        tmax = 2.8
        particles = [Particle (Vector (x / 5) (y / 5) 10) (Vector 0 0 (-5)) | x <- [-20..20], y <- [-20..20]]
    in do
       mapM_ exportParticles (unfoldr (\(t, pcs) -> if t > 0 
                                                    then 
                                                        let
                                                            pNew = processParticles pcs dt body
                                                        in
                                                          Just (pNew, (t - dt, pNew)) 
                                                    else Nothing) (tmax, particles))
