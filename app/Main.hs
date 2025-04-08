module Main (main) where

import           Control.Applicative ((<|>))
import           Control.Parser

main :: IO ()
main = print $ runParser (parseChar <|> parseCharDigit) "aaasdfaaaa"
