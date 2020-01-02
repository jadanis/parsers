import Parser
import Control.Applicative

morseCode :: String -> Char
morseCode s
  case s of
    ".-" -> 'a'
    "-..." -> 'b'
    "-.-." -> 'c'
    "-.." -> 'd'
    "." -> 'e'
    "..-." -> 'f'
    "--." -> 'g'
    "...." -> 'h'
    ".." -> 'i'
    ".---" -> 'j'
    "-.-" -> 'k'
    ".-.." -> 'l'
    "--" -> 'm'
    "-." -> 'n'
    "---" -> 'o'
    ".--." -> 'p'
    "--.-" -> 'q'
    ".-." -> 'r'
    "..." -> 's'
    "-" -> 't'
    "..-" -> 'u'
    "...-" -> 'v'
    ".--" -> 'w'
    "-..-" -> 'x'
    "-.--" -> 'y'
    "--.." -> 'z'
    ".----" -> '1'
    "..---" -> '2'
    "...--" -> '3'
    "....-" -> '4'
    "....." -> '5'
    "-...." -> '6'
    "--..." -> '7'
    "---.." -> '8'
    "----." -> '9'
    "-----" -> '0'
    _ -> error "unknown code sequence"

dot :: Parser Char
dot = char '.'

dash :: Parser Char
dash = char '-'

morseCodeP :: Parser Char
morseCodeP = morseCode <$> (some (dot <|> dash))

morseWordP :: Parser String
morseWordP = ws *> many morseCodeP <* ws
