{-# LANGUAGE NoImplicitPrelude #-}

module Optimizer
    (
    optimize
    ) where

import Protolude
import Parser (Token(..))


optimize :: [Token] -> Maybe [Token]
optimize xs = Just $ optimize' xs
    where
        optimize' :: [Token] -> [Token]
        optimize' (Change x:Change y:xs)  = optimize' (Change (x + y) : xs)
        optimize' (Move x:Move y:xs)      = optimize' (Move (x + y) : xs)
        optimize' (Loop [Change (-1)]:xs) = Clear : optimize' xs
        optimize' (Loop xs:xss)           = Loop (optimize' xs) : optimize' xss
        optimize' (x:xs)                  = x : optimize' xs
        optimize' []                      = []
