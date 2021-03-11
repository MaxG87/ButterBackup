{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}  -- To derive Show
{-# LANGUAGE TypeOperators      #-}

import Options.Generic
import System.Process (readProcess)

data Example w = Example
    { foo :: w ::: Int    <?> "Documentation for the foo flag"
    , bar :: w ::: Double <?> "Documentation for the bar flag"
    } deriving (Generic)

instance ParseRecord (Example Wrapped)
deriving instance Show (Example Unwrapped)

main = do
    x <- unwrapRecord ""  -- string argument is error message
    print (x :: Example Unwrapped)

    results <- splitLines runShell
    -- print each line in reverse
    mapM_ (putStrLn . reverse) results

runShell :: IO String
runShell = readProcess "./hello-world.sh" [] []

splitLines :: IO String -> IO [String]
splitLines stdout = fmap lines stdout  -- fmap a b == a <$> b
