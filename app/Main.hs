{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import System.Directory (doesFileExist)

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [[Char]] -> IO ()
parseArgs []     = repl
parseArgs ["-i"] = repl
parseArgs [name] = doesFileExist name >>= (\x ->
    if x == True
        then readFile name >>= run
        else putText "Ti dibil, sore")

run :: Text -> IO ()
run = undefined
-- run the programm

repl :: IO ()
repl = undefined
-- run REPL
