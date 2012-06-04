module Main

where

import System.Random.MWC

import DSMC.Constants
import DSMC.Domain
import DSMC.Particles

main :: IO ()
main =
    let
        dt = 0.0001
        tmax = 0.01
        domain = Box (-1) 1 (-1) 1 (-1) 1
        t = 300
        n = 100000
        m = 29 * amu
        flow = (10, 0, 0)
    in do
      g <- create
      e <- spawnParticles g domain n t m flow
      printEnsemble e
