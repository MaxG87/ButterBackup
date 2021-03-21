{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as Text
import           GHC.Generics
import           System.Environment
import           System.Exit
import           System.Process       (readProcess)

data ButterConfig =
  ButterConfig
    { uuid    :: !Text.Text
    , passCmd :: !Text.Text
    }
  deriving (Show, Generic)

instance FromJSON ButterConfig

instance ToJSON ButterConfig

main :: IO ()
main = do
  config <- fmap parseArgs getArgs
  join $ fmap print config
  results <- fmap lines runShell
  mapM_ (putStrLn . reverse) results

parseArgs :: [String] -> IO B.ByteString
parseArgs ["-h"]               = printUsage >> exit
parseArgs ["--help"]           = printUsage >> exit
parseArgs ["-v"]               = printVersion >> exit
parseArgs ["--version"]        = printVersion >> exit
parseArgs []                   = readConfig "~/.config/butter-backup.cfg"
parseArgs ("--config":path:[]) = readConfig path
parseArgs _                    = printUsage >> Main.die

readConfig :: String -> IO B.ByteString
readConfig path = B.readFile path

printUsage :: IO ()
printUsage = putStrLn "Usage"

printVersion :: IO ()
printVersion = putStrLn "Usage"

exit :: IO a
exit = exitWith ExitSuccess

die :: IO a
die = exitWith $ ExitFailure 1

runShell :: IO String
runShell = readProcess "./hello-world.sh" [] []
