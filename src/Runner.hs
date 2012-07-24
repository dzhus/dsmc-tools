{-# LANGUAGE BangPatterns #-}

module Main

where

import Data.ConfigFile
import Control.Monad.Error

import DSMC
import DSMC.Domain
import DSMC.Particles
import DSMC.Traceables
import DSMC.Util.Constants

import GHC.Conc.Sync

simSection :: String
simSection = "Simulation"


flowSection :: String
flowSection = "Flow"


domainSection :: String
domainSection = "Domain"


macroSection :: String
macroSection = "Macroscopic"


main :: IO ()
main =
    let
        origin = (0, 0, 0)
        body = (sphere (0, 0, 0) 0.3)
    in do
      split <- getNumCapabilities
      res <- runErrorT $ do
               cp <- join $ liftIO $ readfile emptyCP "example.tsk"
               n <- get cp flowSection "n"
               t <- get cp flowSection "t"
               m <- get cp flowSection "m"
               v <- get cp flowSection "velocity"
               sw <- get cp flowSection "fn"
               w <- get cp domainSection "w"
               l <- get cp domainSection "l"
               h <- get cp domainSection "h"
               ex <- get cp simSection "ex"
               dt <- get cp simSection "dt"
               ssteps <- get cp simSection "steady_steps"
               sepsilon <- get cp simSection "steady_epsilon"
               emptys <- get cp simSection "empty_start"
               mx <- get cp macroSection "mx"
               my <- get cp macroSection "my"
               mz <- get cp macroSection "mz"
               return $ (Flow n t (m * amu) v sw, ex, dt, ssteps, sepsilon, emptys, mx, my, mz,
                         makeDomain origin w l h)
      case res of
        Left e -> print e
        Right (flow, ex, dt, ssteps, sepsilon, emptys, mx, my, mz, domain) -> do
                 -- s1 <- create >>= save
                 -- s2 <- create >>= save
                 -- s3 <- create >>= save
                 -- s4 <- create >>= save
                 -- s5 <- create >>= save
                 -- s6 <- create >>= save
                 -- let !(e, _) = openBoundaryInjection (s1, s2, s3, s4, s5, s6) domain ex flow emptyEnsemble
                 !e <- simulate domain body flow dt emptys ex sepsilon ssteps (mx, my, mz) split
--                 printEnsemble e
                 print "Finished!"
--                 print $ R.extent e
                 return ()
