module Hexagony
(
)
where

import Parser
import Control.Applicative ((<|>),many)

data HexCom
  = NoOp -- .
  | Terminate -- @
  | Shift Int -- 0-9
  | Inc --)
  | Dec -- (
  | Sum -- +
  | Diff -- -
  | Prod -- *
  | Quot -- :
  | Mod -- %
  | Neg -- ~
  | ByteRead -- ,
  | IntRead -- ?
  | ByteOut -- ;
  | IntOut -- !
  | Jump -- $
  | NSMirr -- _
  | WEMirr -- |
  | DMirr -- /
  | D'Mirr -- \
  | LBranch -- <
  | RBranch -- >
  | DecIP -- [
  | IncIP -- ]
  | ModIP -- # mod 6
  | LMP -- {
  | RMP -- }
  | BLMP -- "
  | BRMP -- '
  | RevMP -- =
  | BranchMP -- ^
  | CopyMP -- &
  deriving (Read,Show,Eq)

op :: HexCom -> Char -> Parser HexCom
op hx c = (const hx) <$> (char c)

shiftInt :: Int -> Parser HexCom
shiftInt n = op (Shift n) (head $ show n)

opList' :: [Parser HexCom]
opList' = zipWith op [NoOp, Terminate, Inc, Dec, Sum, Diff, Prod, Quot, Mod, Neg, ByteRead, IntRead, ByteOut, IntOut, Jump, NSMirr, WEMirr, DMirr, D'Mirr, LBranch, RBranch, DecIP, IncIP, ModIP, LMP,RMP, BLMP, BRMP, RevMP, BranchMP, CopyMP] ['.','@',')','(','+','-','*',':','%','~',',','?',';','!','$','_','|','/','\\','<','>','[',']','#','{','}','"','\'', '=', '^','&']

opList :: [Parser HexCom]
opList = (map shiftInt [0..9]) ++ opList'

hexcom :: Parser HexCom
hexcom = oneOf opList
