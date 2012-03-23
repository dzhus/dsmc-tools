{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-|
  
Standalone raycasting runner for traceable objects.
  
|-}

import DSMC.Traceables
import DSMC.Traceables.Raycast
import DSMC.Traceables.SexParser

import System.Console.CmdArgs.Implicit

data Options = Options
    { bodyDef :: Maybe FilePath }
    deriving (Data, Typeable)

main :: IO ()
main = 
    let
        sample = Options
                 { bodyDef = def
                   &= help "Path to body definition file"
                 }
    in do
      Options{..} <- cmdArgs $ sample
      case bodyDef of
        Just fname -> print fname
        Nothing -> error "Lol u suck mon"
