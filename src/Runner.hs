{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main

where

import Control.Monad.Error
import Control.Exception

import Data.ByteString.Char8 as B

import Data.ConfigFile

import Data.Functor

import Data.Vector.Unboxed as VU hiding ((++))

import System.Console.CmdArgs.Implicit

import System.IO

import System.Log
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple

import DSMC
import DSMC.Domain
import DSMC.Macroscopic
import DSMC.Particles
import DSMC.Surface
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


surfaceSection :: String
surfaceSection = "Surface"


-- | Command-line options for the runner.
data Options = Options
    { probDef :: FilePath
    , macroFile :: Maybe FilePath
    , ensFile :: Maybe FilePath
    }
    deriving (Data, Typeable)

main :: IO ()
main =
  let
    sample = Options
             { probDef = def &= argPos 0 &= typ "PROBLEM-FILE"
             , macroFile = Nothing &= help "Macroscopic data output file"
             , ensFile = Nothing &= help "Ensemble data output file"
             }
             &= program "dsmc-runner"
  in do
    -- | Setup logging to stderr
    let lf = simpleLogFormatter "$time: [$prio] $msg"
    lh <- (flip setFormatter) lf <$> verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [lh] )

    debugM rootLoggerName "Program started"

    Options{..} <- cmdArgs $ sample

    debugM rootLoggerName ("Using problem file: " ++ probDef)

    debugM rootLoggerName $
           maybe "Not writing macroscopic data"
           ("Macroscopic data output file: " ++) macroFile

    debugM rootLoggerName $
           maybe "Not writing ensemble data"
           ("Ensemble data output file: " ++) ensFile

    -- Use number of cores as split factor for parallelizing
    -- stochastic processes.
    nsplit <- getNumCapabilities

    (hr :: Either IOException Handle) <- try (openFile probDef ReadMode)
    case hr of
      Left e -> errorM rootLoggerName $ show e
      Right hdl -> do
        res <- runErrorT $ do
                 cp <- join $ liftIO $ readhandle emptyCP hdl
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
                 vpts <- get cp bodySection "volume_points"
                 mx <- get cp macroSection "mx"
                 my <- get cp macroSection "my"
                 mz <- get cp macroSection "mz"
                 bodyDef <- get cp bodySection "definition"
                 -- Interface to diffuse model only
                 surfTemp <- get cp surfaceSection "temperature"
                 return $ (Flow n t (m * amu) v sw, ex, dt,
                           ssteps, sepsilon, emptys,
                           vpts,
                           mx, my, mz,
                           makeDomain origin w l h,
                           bodyDef,
                           surfTemp)
        case res of
          Left e -> errorM rootLoggerName $
                    "Problem when reading problem definition: " ++ show e
          Right (flow, ex, dt,
                 ssteps, sepsilon, emptys,
                 vpts,
                 mx, my, mz,
                 domain,
                 bodyDef,
                 surfTemp) -> do
                   -- Try to read body from definition file
                   debugM rootLoggerName $
                          "Using body definition file: " ++ bodyDef
                   body <- parseBodyFile bodyDef
                   case body of
                     Left e -> errorM rootLoggerName $
                               "Problem when reading body definition: " ++ e
                     Right b ->
                       do
                         debugM rootLoggerName $ "Starting simulation"
                         -- Run the simulation, obtaining
                         -- iteration count and final
                         -- distributions
                         !(iters, e, macro) <-
                           simulate domain b flow dt
                                    emptys ex sepsilon ssteps
                                    (Diffuse surfTemp $
                                     DSMC.Particles.mass flow)
                                    (mx, my, mz) vpts nsplit
                         debugM rootLoggerName "Simulation complete"
                         debugM rootLoggerName $
                                "Particle count when finished: " ++
                                (show $ ensembleSize e)
                         debugM rootLoggerName $
                                "Total iterations done: " ++
                                (show iters)
                         case macroFile of
                           Just fp -> do
                             debugM rootLoggerName $
                               "Writing macroscopic data to file file: " ++ fp
                             saveMacroscopic macro fp
                           Nothing -> return ()
                         case ensFile of
                           Just fp -> do
                             debugM rootLoggerName $
                               "Writing ensemble data to file: " ++ fp
                             saveEnsemble e fp
                           Nothing -> return ()
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
         -> ByteString
         -- ^ Header line
         -> R.Array U sh e
         -> FilePath
         -> IO ()
saveRepa f header array path = do
    h <- openFile path WriteMode
    hPut h $ B.concat [header, nl]
    VU.forM_ (R.toUnboxed array) (hPut h . f)


formatParticle :: Particle -> ByteString
formatParticle ((x, y, z), (u, v, w)) =
    B.concat [fd x, ws, fd y, ws, fd z, ws, fd u, ws, fd v, ws, fd w, nl]
{-# INLINE formatParticle #-}


formatMacroParameters :: (Point, IntensiveMacroParameters) -> ByteString
formatMacroParameters ((x, y, z), (n, (u, v, w), p, t)) =
    if n == 0.0 then B.empty
    else B.concat [fd x, ws, fd y, ws, fd z, ws,
                   fd n, ws,
                   fd u, ws, fd v, ws, fd w, ws,
                   fd p, ws, fd t, nl]
{-# INLINE formatMacroParameters #-}


saveMacroscopic :: MacroField -> FilePath -> IO ()
saveMacroscopic =
  saveRepa formatMacroParameters $
  B.pack "x y z number_density u_avg v_avg w_avg pressure trans_temp"


saveEnsemble :: Ensemble -> FilePath -> IO ()
saveEnsemble = saveRepa formatParticle $ B.pack "x y z u v w"
