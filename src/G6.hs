import Parser
import Control.Applicative
import Data.Char

data G6
  = Graph Int [(Int,Int)]
  deriving (Show, Read, Eq)

order :: Parser Int
order = t1 <|> t2 <|> t3
  where
    t1 = ((subtract 63) . ord) <$> (oneOf $ map char ['?'..'}'])
    t2 = (foldl (\acc x -> acc*64 + x) 0) <$> (char '~' *> (3 `times` t1) )
    t3 = (foldl (\acc x -> acc*64 + x) 0) <$> (char '~' *> char '~' *> (6 `times` t1) )

bits :: Int -> Int -> String
bits 0 _ = ""
bits n m = (bits (n-1) (m `div` 2)) ++ ((if m `mod` 2 == 0 then '0' else '1'):[])

bitvec :: Parser String
bitvec = ((bits 6) . (subtract 63) . ord) <$> (oneOf $ map char ['?'..'~'])

enum' :: (Int,Int) -> (Int,Int)
enum' (n,m)
  | (n+1) == m = (0,m+1)
  | n < m = (n+1,m)
  | otherwise = error "Whoops!"

toMatrix :: String -> [(Int,Int)]
toMatrix = toMatrix' (0,1)

toMatrix' :: (Int,Int) -> String -> [(Int,Int)]
toMatrix' tup s = 
  case s of 
    "" -> []
    (b:bs) | b == '1' -> tup:(toMatrix' (enum' tup) bs)
    (b:bs) | b == '0' -> toMatrix' (enum' tup) bs
    _ -> error "Unexpected bit in bitstring"

adj_matrix :: Int -> Parser G6
adj_matrix n = ((Graph n) . toMatrix . reverse . (drop dm) . reverse . concat ) <$> (m `times` bitvec)
  where
    bin_n = n*(n-1) `div` 2
    m' = bin_n `mod` 6
    m = (bin_n `div` 6) + (if m' == 0 then 0 else 1)
    dm = if m' == 0 then 0 else (6 - m')

g6 :: Parser G6
{-g6 = Parser $ \input -> do
  (n,input') <- parse order input
  (mat,input'') <- parse (adj_matrix n) input'
  return (Graph n mat, input'')
-}
g6 = order >>= adj_matrix

main :: IO ()
main = do
  let res = parse g6 "MAbaHfLt}dk]mf~{_"
  putStrLn $ show res
