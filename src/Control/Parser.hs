module Control.Parser
    ( Parser (..)
    , zeroParser
    , parseChar
    , parseCharConditional
    , parseCharEq
    , parseCharDigit
    , parseString
    ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Monad       (MonadPlus, mplus, mzero)
import           Data.Bifunctor      (first)
import           Data.Char           (isDigit)
import           Data.Text           (Text)
import qualified Data.Text           as Text

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

instance MonadPlus Parser where
    mzero = empty

    mplus p q = Parser $ \t -> runParser p t ++ runParser q t

instance Alternative Parser where
    empty = mzero

    p <|> q = Parser $ \t ->
        case runParser (mplus p q) t of
            [] -> []
            xs -> [head xs]

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

parseCharEq :: Char -> Parser Char
parseCharEq = parseCharConditional . (==)

parseCharDigit :: Parser Char
parseCharDigit = parseCharConditional isDigit

parseString :: Text -> Parser Text
parseString "" = return ""
parseString t =
    let x = Text.head t; xs = Text.drop 1 t in
        parseCharEq x >> parseString xs >> return (Text.cons x xs)
