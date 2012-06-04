module Main

where

import System.Random.MWC

import Data.ConfigFile
import Control.Monad.Error

import DSMC.Constants
import DSMC.Domain
import DSMC.Particles


flowSection :: String
flowSection = "Flow"


domainSection :: String
domainSection = "Domain"


main :: IO ()
main =
    let
        origin = (0, 0, 0)
    in do
      res <- runErrorT $ do
               cp <- join $ liftIO $ readfile emptyCP "example.tsk"
               n <- get cp flowSection "n"
               t <- get cp flowSection "t"
               m <- get cp flowSection "m"
               w <- get cp domainSection "w"
               l <- get cp domainSection "l"
               h <- get cp domainSection "h"
               v <- get cp flowSection "flowVelocity"
               return $ (Flow n t (m * amu) v, 
                         makeDomain origin w l h)
      case res of
        Left e -> print e
        Right (flow, domain) -> do
                 print flow
                 g <- create
                 e <- spawnParticles g domain flow
                 printEnsemble $ fromUnboxed1 e
