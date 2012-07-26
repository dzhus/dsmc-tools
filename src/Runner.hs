{-# LANGUAGE BangPatterns #-}

module Main

where

import Control.Monad.Error

import Data.ByteString.Char8 as B

import Data.ConfigFile

import Data.Vector.Unboxed as VU

import System.IO

import DSMC
import DSMC.Domain
import DSMC.Macroscopic
import DSMC.Particles
import DSMC.Traceables
import DSMC.Util.Constants
import DSMC.Util.Vector

import Data.Array.Repa as R

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
        -- TODO Load body from external definition
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
               -- TODO Tidy this up
               return $ (Flow n t (m * amu) v sw, ex, dt, ssteps, sepsilon, emptys, mx, my, mz,
                         makeDomain origin w l h)
      case res of
        Left e -> print e
        Right (flow, ex, dt, ssteps, sepsilon, emptys, mx, my, mz, domain) -> do
                 !(e, macro) <- simulate domain body flow dt emptys ex sepsilon ssteps (mx, my, mz) split
                 print $ R.extent macro
                 print "Finished!"
                 _ <- saveMacroscopic macro "macro.txt"
                 _ <- saveEnsemble e "ensemble.txt"
                 return ()


fd :: Double -> ByteString
fd = pack . show
{-# INLINE fd #-}


ws :: ByteString
ws = B.singleton ' '


nl :: ByteString
nl = B.singleton '\n'


-- | Save Repa array in text file, one element per line.
saveRepa :: (Unbox e) => 
            (e -> ByteString)
         -- ^ Formatter function.
         -> R.Array U sh e
         -> FilePath
         -> IO ()
saveRepa f array path = do
    h <- openFile path WriteMode
    VU.forM_ (R.toUnboxed array) (hPut h . f)


formatParticle :: Particle -> ByteString
formatParticle ((x, y, z), (u, v, w)) =
    B.concat [fd x, ws, fd y, ws, fd z, ws, fd u, ws, fd v, ws, fd w, nl]
{-# INLINE formatParticle #-}


formatMacroParameters :: (Point, MacroParameters) -> ByteString
formatMacroParameters ((x, y, z), (n, (u, v, w), c)) =
    if n == 0.0 then B.empty
    else B.concat [fd x, ws, fd y, ws, fd z, ws, fd u, ws, fd v, ws, fd w, nl]
{-# INLINE formatMacroParameters #-}


saveMacroscopic :: MacroField -> FilePath -> IO ()
saveMacroscopic = saveRepa formatMacroParameters


saveEnsemble :: Ensemble -> FilePath -> IO ()
saveEnsemble = saveRepa formatParticle
