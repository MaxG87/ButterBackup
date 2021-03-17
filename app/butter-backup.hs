import Control.Monad
import System.Environment
import System.Process (readProcess)

data CLIArguments = PrintHelp | FilePath
    deriving (Show)
newtype CLIArgParseError = CLIArgParseError String
    deriving (Show)
type CLIArgParseResult = Either CLIArguments CLIArgParseError

main :: IO ()
main = join $ fmap printArgs args
    where args = fmap parseArgs getArgs

printArgs :: CLIArgParseResult -> IO ()
printArgs args = print $ case args of
        Left PrintHelp -> "Help"
        Left FilePath -> "foobar"
        Right (CLIArgParseError str) -> str

    -- x <- getArgs
    -- print $ case x of
    --     (x:xs) -> x
    --     [] -> "Empty Args"

    -- my_variable <- parseArgs x
    -- print $ case x of
    --     (x:xs) -> "blablubb"
    --     [] -> "Again empty args"

    -- results <- splitLines runShell
    -- -- print each line in reverse
    -- mapM_ (putStrLn . reverse) results


parseArgs :: [String] -> CLIArgParseResult
parseArgs x = case x of
    ["-h"] -> Left PrintHelp
    ["--help"] -> Left PrintHelp
    ("--config":_:[]) -> Left FilePath 
    _ -> Right $ CLIArgParseError "CLIArgParseError"
-- parseArgs ["-h"]  = Left (printUsage >> exitWith ExitSuccess)
-- parseArgs ["--help"]  = Left (printUsage >> exitWith ExitSuccess)
-- parseArgs ["--config", path] = Right path
-- parseArgs _  = error "ABC"

-- printUsage = putStrLn "Usage"

-- runShell :: IO String
-- runShell = readProcess "./hello-world.sh" [] []

-- splitLines :: IO String -> IO [String]
-- splitLines stdout = fmap lines stdout  -- fmap a b == a <$> b
