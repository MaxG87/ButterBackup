import System.Process (readProcess)

main :: IO ()
main = do
    results <- splitLines runShell
    -- print each line in reverse
    mapM_ (putStrLn . reverse) results

runShell :: IO String
runShell = readProcess "./hello-world.sh" [] []

splitLines :: IO String -> IO [String]
splitLines stdout = fmap lines stdout  -- fmap a b == a <$> b
