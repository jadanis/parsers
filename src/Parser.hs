{-# LANGUAGE LambdaCase #-}

module Parser
( Parser (..)
, parse
, char
, string
, jsTrue
, jsValue
, jsArray
, jsObject
, jsNumber
, element
, integer
, oneOf
, ws
, Json (..)
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

element :: Parser Json
element = ws *> jsValue <* ws

data Json 
    = JsBool Bool 
    | JsNull
    | JsString String
    | JsNumber Integer
    | JsArray [Json]
    | JsObject [(String,Json)]
    deriving (Show, Eq)

jsTrue :: Parser Json
jsTrue = Parser $ \text -> do
    (_, rest) <- parse (string "true") text
    pure (JsBool True,rest)

jsFalse :: Parser Json
jsFalse = Parser $ \text -> do
    (_, rest) <- parse (string "false") text
    pure (JsBool False,rest)

jsNull :: Parser Json    
jsNull = Parser $ \text -> do
    (_, rest) <- parse (string "null") text
    pure (JsNull,rest)

jsString :: Parser Json
jsString = JsString <$> between (many character) (char '"') (char '"')
    where
        character = escapedQuote <|> anyChar `unless` (== '"')
        escapedQuote = char '\\' *> char '"'

jsNumber :: Parser Json
jsNumber = JsNumber <$> integer

jsValue :: Parser Json
jsValue = jsObject <|> jsArray <|> jsString <|> jsNumber <|> jsTrue <|> jsFalse <|> jsNull

jsArray :: Parser Json
jsArray = JsArray <$> (between (const [] <$> ws) (char '[') (char ']')
        <|> between elements (char '[') (char ']'))
        where
            elements = manySepBy (ws *> jsValue <* ws) (char ',')

jsObject :: Parser Json
jsObject = JsObject <$> (between (const [] <$> ws) (char '{') (char '}')
        <|> between members (char '{') (char '}'))
        where
            members = manySepBy member (char ',')
            member = do
                ws
                jsKey <- jsString
                ws
                char ':'
                value <- element
                case jsKey of
                    JsString key -> pure (key, value)
                    _ -> empty
                

digit :: Parser Integer
digit = oneOf $ (\(v,c) -> const v <$> char c) <$> zip [0..] ['0'..'9']

unsignedInteger :: Parser Integer
unsignedInteger = foldr (\d r -> d + r*10) 0 . reverse <$> some digit

integer :: Parser Integer
integer = unsignedInteger <|> negativeInteger
        where
            negativeInteger = (*) (-1) <$> (char '-' *> unsignedInteger)


