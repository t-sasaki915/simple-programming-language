module Control.Parser
    ( Parser (..)
    , zeroParser
    , parseChar
    , parseCharConditional
    ) where

import           Data.Bifunctor (first)
import           Data.Text      (Text)
import qualified Data.Text      as Text

newtype Parser a = Parser { parse :: Text -> [(a, Text)] }

instance Functor Parser where
    fmap f pa = Parser $ \text -> fmap (first f) (parse pa text)

instance Applicative Parser where
    pure v = Parser $ \text -> [(v, text)]

    pf <*> pa = Parser $ \text -> do
        (f, text')  <- parse pf text
        (a, text'') <- parse pa text'
        return (f a, text'')

instance Monad Parser where
    pa >>= f = Parser $ \text ->
        concat [parse (f v) text' | (v, text') <- parse pa text]

zeroParser :: Parser a
zeroParser = Parser $ const []

parseChar :: Parser Char
parseChar = Parser parseItem'
    where
        parseItem' text
            | Text.null text = []
            | otherwise      = [(Text.head text, Text.drop 1 text)]

parseCharConditional :: (Char -> Bool) -> Parser Char
parseCharConditional cond =
    parseChar >>=
        \x -> if cond x then return x else zeroParser
