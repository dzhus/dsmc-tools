{-# LANGUAGE BangPatterns #-}

module Main

where

import Control.Monad.Error

import Data.ByteString.Char8 as B

import Data.ConfigFile

import Data.Vector.Unboxed as VU hiding ((++))

import System.IO
import System.Environment

import DSMC
import DSMC.Domain
import DSMC.Macroscopic
import DSMC.Particles
import DSMC.Traceables
import DSMC.Traceables.Parser
import DSMC.Util.Constants
import DSMC.Util.Vector

import Data.Array.Repa as R hiding ((++))

import GHC.Conc.Sync



simSection :: String
simSection = "Simulation"


flowSection :: String
flowSection = "Flow"


domainSection :: String
domainSection = "Domain"


macroSection :: String
macroSection = "Macroscopic"

bodySection :: String
bodySection = "Body"


main :: IO ()
main =
    let
        origin = (0, 0, 0)
    in do
      [probDef, macroFile, ensFile] <- getArgs
      split <- getNumCapabilities
      print $ "Using problem definition file: " ++ probDef
      res <- runErrorT $ do
               cp <- join $ liftIO $ readfile emptyCP probDef
               -- Must refactor this somehow (actually, write CmdArgs
               -- for .ini files)
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
               bodyDef <- get cp bodySection "definition"
               return $ (Flow n t (m * amu) v sw, ex, dt, 
                         ssteps, sepsilon, emptys, 
                         mx, my, mz,
                         makeDomain origin w l h,
                         bodyDef)
      case res of
        Left e -> print e
        Right (flow, ex, dt, 
               ssteps, sepsilon, emptys, 
               mx, my, mz, 
               domain, 
               bodyDef) -> do
                 -- Try to read body from definition file
                 body <- parseBodyFile bodyDef
                 case body of
                   Left e -> error $ "Problem when reading body definition " ++ bodyDef ++ ": " ++ e
                   Right b -> do
                             -- Run the simulation, obtaining iteration count and final distributions
                             !(iters, e, macro) <- simulate domain b flow dt emptys ex sepsilon ssteps (mx, my, mz) split
                             print $ R.extent macro
                             print $ "Finished after " ++ (show iters) ++ " iterations"
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
