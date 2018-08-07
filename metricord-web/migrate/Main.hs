module Main where

import Prelude

import Control.Concurrent

import Metricord.Migrate

main :: IO ()
main = do
    putStrLn "Running migrations . . ."
    threadDelay 100000
    putStr ". "
    threadDelay 100000
    putStr ". "
    threadDelay 100000
    putStr ". "
    threadDelay 100000
    putStrLn "er, not implemented yet!"

