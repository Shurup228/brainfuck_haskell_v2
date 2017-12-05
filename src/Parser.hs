{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
    (
    parse
  , Token(..)
    ) where

import           Protolude
import qualified Data.Text as T (uncons, snoc, filter)

data Token = Move Int
           | Change Int
           | Print
           | Put
           | Loop [Token] deriving (Show, Eq)


syntax :: Text -> Bool
syntax = go 0 . toS . T.filter (`elem` ['[', ']'])
    where
        go :: Int -> [Char] -> Bool
        go (-1) _       = False
        go acc ('[':xs) = go (acc + 1) xs
        go acc (']':xs) = go (acc - 1) xs
        go 0 _          = True
        go 1 _          = False

parse' :: Text -> [Token]
parse' ""   = []
parse' code = fromMaybe ([Move 1]) $ T.uncons code >>=
    (\(op, c) ->
    return (case op of
        '>' -> Move 1          : parse' c
        '<' -> Move (-1)       : parse' c
        '+' -> Change 1        : parse' c
        '-' -> Change (-1)     : parse' c
        '.' -> Print           : parse' c
        ',' -> Put             : parse' c
        '[' -> let (loop, remainder) = getLoop 1 "" c in
            Loop (parse' loop) : parse' remainder
        _  -> parse' c))
            where
                getLoop :: Int -> Text -> Text -> (Text, Text)
                getLoop  0  loop code = (loop, code)
                getLoop acc loop code = fromMaybe (("", "")) $
                    T.uncons code >>= (\(op, c) ->
                    return (case op of
                        '[' -> getLoop (acc + 1) (T.snoc loop op) c
                        ']' -> getLoop (acc - 1) (T.snoc loop op) c
                        _  ->  getLoop    acc    (T.snoc loop op) c))

parse :: Text -> Maybe [Token]
parse code = case syntax code of
    True  -> Just (parse' code)
    False -> Nothing
