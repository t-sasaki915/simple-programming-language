module Control.Parser
    ( Parser (..)
    , zeroParser
    , parseChar
    , parseCharConditional
    ) where

import           Data.Bifunctor (first)
import           Data.Text      (Text)
import qualified Data.Text      as Text

newtype Parser a = Parser { runParser :: Text -> [(a, Text)] }

instance Functor Parser where
    fmap f pa = Parser $ map (first f) . runParser pa

instance Applicative Parser where
    pure v = Parser $ \t -> [(v, t)]

    pf <*> pa = Parser $ \t -> do
        (f, t')  <- runParser pf t
        (a, t'') <- runParser pa t'
        return (f a, t'')

instance Monad Parser where
    pa >>= f = Parser $ \t ->
        concat [runParser (f v) t' | (v, t') <- runParser pa t]

zeroParser :: Parser a
zeroParser = Parser $ const []

parseChar :: Parser Char
parseChar = Parser parseItem'
    where
        parseItem' t
            | Text.null t = []
            | otherwise   = [(Text.head t, Text.drop 1 t)]

parseCharConditional :: (Char -> Bool) -> Parser Char
parseCharConditional cond =
    parseChar >>=
        \x -> if cond x then return x else zeroParser
