import Parser
import Control.Applicative

module Equation
( Equation (..)
, Term (..)
, interpret
, eval
, eqP
)
where

type Equation = [Term]

data Term
  = E Float
  | RParan
  | LParan
  | Exp
  | Mult
  | Div
  | Add
  | Sub
  deriving (Show, Eq, Read)

interpret :: String -> String
interpret input = 
  case parse eqP input of
    Nothing -> "Could not parse input"
    Just (eq,rs) -> show $ eval eq

eval :: Equation -> Float
eval = eval' []

eval' :: [Float] -> Equation -> Float
eval' stack eq =
  case eq of
    [] ->
      case stack of
        [] -> error "No equation to evaluate"
        [x] -> x
        _ -> error "Parsing error at evaluation attempt"
    (t:ts) ->
      case t of
        E fl -> eval' (fl:stack) ts
        op -> 
          case stack of
            [] -> error "Parsing error, no operands"
            [f] -> error "Parsing error, only 1 operand"
            (f1:f2:fs) -> eval' (((operate op) f2 f1):fs) ts

operate :: Term -> (Float -> Float -> Float)
operate o =
  case o of
    Exp -> (**)
    Mult -> (*)
    Div -> (/)
    Add -> (+)
    Sub -> (-)
    _ -> error "Token should not be a number"

precedence :: Term -> Int
precedence op =
  case op of
    Exp -> 1
    Mult -> 2
    Div -> 2
    Add -> 3
    Sub -> 3
    _ -> 4

eqP :: Parser Equation
eqP = eqHelper <$> (some termP)

eqHelper :: Equation -> Equation
eqHelper u = eqHelper' u [] [] 

eqHelper' :: Equation -> Equation -> Equation -> Equation
eqHelper' unread output stack =
  case unread of
    [] -> (reverse output) ++ stack
    (t:ts) -> 
      case t of
        E fl -> eqHelper' ts (t:output) stack
        LParan -> eqHelper' ts output (LParan:stack)
        RParan -> eqHelper' ts ((reverse $ takeWhile (/= LParan) stack)++output) (tail $ dropWhile (/= LParan) stack)
        o ->
          case stack of
            (o1:os) | (precedence o == precedence o1) -> eqHelper' ts (o1:output) (o:os)
            _ -> eqHelper' ts output (o:stack)

termP :: Parser Term
termP = oneOf [rparanP, lparanP,expP,multP,divP,addP,subP,floatP]

rparanP :: Parser Term
rparanP = opP RParan ')'

lparanP :: Parser Term
lparanP = opP LParan '('

expP :: Parser Term
expP = opP Exp '^'

multP :: Parser Term
multP = opP Mult '*'

divP :: Parser Term
divP = opP Div '/'

addP :: Parser Term
addP = opP Add '+'

subP :: Parser Term
subP = opP Sub '-'

opP :: Term -> Char -> Parser Term
opP t c = (const t) <$> (ws *> char c <* ws)

floatP :: Parser Term
floatP = E <$> float
