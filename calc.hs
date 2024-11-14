
import System.IO
import Data.Char


main :: IO()
main = calcCycle []

calcCycle :: [Double] -> IO()
calcCycle prev = do
   putStrLn "Please enter an expression"
   expression <- getLine
   case calculate (reverse (split expression)) [] prev of
      Nothing -> do
         putStrLn "Bad Input"
         calcCycle prev
      Just answer -> do
         putStrLn (show (1 + length prev)++ " "++ show answer)
         calcCycle (prev ++ [answer])


isOperator :: String -> Bool
isOperator "+" = True
isOperator "*" = True
isOperator "/" = True
isOperator _ = False

isModifier :: Char -> Bool
isModifier '-' = True
isModifier '$' = True
isModifier _ = False

isNumber :: String -> Bool
isNumber "" = False
isNumber xs  =
  case dropWhile isDigit xs of
    ""       -> True
    _        -> False

split :: String -> [String]
split [] = []
split (a:xs) 
   |isDigit a = let(number, rest) = span isDigit (a:xs) in number: split rest
   |isSpace a = split xs
   |isModifier a = let(number, rest) = span isDigit xs in (a:number): split rest
   |otherwise = [a]:split xs


calculate :: [String] -> [Double] -> [Double] -> Maybe Double
calculate (a:xs) (x:y:rest) prev
   |isOperator a = case applyOp a x y of
                  Nothing -> Nothing
                  Just n -> calculate xs (n : rest) prev
   |otherwise    = case parse a prev of
                  Nothing -> Nothing
                  Just n ->  calculate xs (n:x:y:rest) prev
calculate (a:xs) nums prev
   | not (isOperator a) = case parse a prev of
                  Nothing -> Nothing
                  Just n ->  calculate xs (n:nums) prev
calculate [] [ans] prev = Just ans
calculate list nums prev = Nothing


applyOp :: String -> Double -> Double -> Maybe Double
applyOp "+" x y = Just (x + y)
applyOp "*" x y = Just (x * y)
applyOp "/" x y
   | y == 0 = Nothing
   | otherwise = Just (x / y)

parse :: String -> [Double] -> Maybe Double
parse thing prev
   | Main.isNumber thing =Just (read thing)
   | head thing == '-' && Main.isNumber (tail thing) = Just (negate (read (tail thing)))
   | head thing == '$' && Main.isNumber (tail thing) =
      let index = read (tail thing) :: Int
      in if index <= length prev
        then Just (prev !! (index - 1))
        else Nothing
   | otherwise = Nothing

