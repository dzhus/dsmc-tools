{-# LANGUAGE BangPatterns #-}

module Main

where

import System.Random.MWC

import Data.ConfigFile
import Control.Monad.Error

import Control.Monad.ST (stToIO)

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


gridSection :: String
gridSection = "Grid"


main :: IO ()
main =
    let
        origin = (0, 0, 0)
        body = (sphere (0, 0, 0) 0.5)
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
               tmax <- get cp simSection "tmax"
--               hx <- get cp gridSection "hx"
--               hy <- get cp gridSection "hy"
--               hz <- get cp gridSection "hz"
               return $ (Flow n t (m * amu) v sw, ex, dt, tmax, --hx, hy, hz,
                         makeDomain origin w l h)
      case res of
        Left e -> print e
        Right (flow, ex, dt, tmax, domain) -> do
                 s1 <- create >>= save
                 s2 <- create >>= save
                 s3 <- create >>= save
                 s4 <- create >>= save
                 s5 <- create >>= save
                 s6 <- create >>= save
                 let !(e, _) = openBoundaryInjection (s1, s2, s3, s4, s5, s6) domain ex flow emptyEnsemble
--                 printEnsemble e
                 !e <- simulate domain body flow dt tmax ex split
--                 print $ R.extent e
--                 regPrintVels (domain, hx, hy, hz) e
                 return ()
