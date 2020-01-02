import Parser
import Control.Applicative

module JSON
( jsTrue
, jsValue
, jsArray
, jsObject
, jsNumber
, element
, Json (..)
)
where

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

