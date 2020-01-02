{-# LANGUAGE LambdaCase #-}

module Parser
( Parser (..)
, parse
, char
, string
, integer
, float
, oneOf
, ws
, times
)
where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Control.Monad (guard)

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \text ->
        (\(v,rest) -> (f v, rest)) <$> p text

instance Applicative Parser where
    pure v = Parser $ \text -> Just (v, text)
    pf <*> p = Parser $ \text -> do
        (f, rest) <- parse pf text
        (v, rest') <- parse p rest
        pure $ (f v, rest')

instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser $ \text ->
        parse p1 text <|> parse p2 text

join :: Parser (Parser a) -> Parser a
join p = Parser $ \text -> 
    case parse p text of
        Nothing -> Nothing
        Just (p', rest) -> 
            parse p' rest 

instance Monad Parser where
    p >>= f = join $ f <$> p

unless :: Parser a -> (a -> Bool) -> Parser a
unless p pred = Parser $ \text -> do
    (v, rest) <- parse p text
    guard $ not $ pred v
    pure (v,rest)

anyChar :: Parser Char
anyChar = Parser $ \case
    (c:rest) -> Just (c, rest)
    [] -> Nothing

char :: Char -> Parser Char
char c = anyChar `unless` (/= c)

ws :: Parser String
ws = many $ oneOf [char '\n', char '\r', char ' ', char '\t']

between :: Parser a -> Parser b -> Parser c -> Parser a
between inner open close =
    open *> inner <* close

oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) empty

manySepBy :: Parser a -> Parser b -> Parser [a]
manySepBy p delim = do
    v <- optional p
    d <- optional delim
    case (v,d) of 
        (Just v', Just _) -> do
            vs <- manySepBy p delim
            pure (v':vs)
        (Just v', _) -> pure [v']
        (Nothing, Just _) -> empty
        (Nothing,Nothing) -> pure []

string :: String -> Parser String
string [] = Parser $ \text -> Just ("",text)
string (c:cs) = Parser $ \text -> do
    (_, rest) <- parse (char c) text
    (_, rest') <- parse (string cs) rest
    pure (c:cs, rest')
                
digit :: Parser Integer
digit = oneOf $ (\(v,c) -> const v <$> char c) <$> zip [0..] ['0'..'9']

unsignedInteger :: Parser Integer
unsignedInteger = foldr (\d r -> d + r*10) 0 . reverse <$> some digit

integer :: Parser Integer
integer = unsignedInteger <|> negativeInteger
        where
            negativeInteger = (*) (-1) <$> (char '-' *> unsignedInteger)


times :: Int -> Parser a -> Parser [a]
times n p
  | n < 0 = error "Cannot parse negative instances of a token"
  | n == 0 = pure []
  | otherwise = liftA2 (:) p (times (n-1) p)

dig :: Parser Char
dig = oneOf $ map char ['0'..'9']

float :: Parser Float
float = ufloat <|> negfloat <|> (fromIntegral <$> integer)
  where
    ufloat = read <$> ((\x y z -> x ++ [y] ++ z) <$> (some dig) <*> (char '.') <*> (some dig))
    negfloat = (*) (-1) <$> (char '-' *> ufloat)
