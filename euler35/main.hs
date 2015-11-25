module Main where
-- Generalized Project Euler problem 1 solution

main :: IO ()
main = mapM_ printline [1..9]

printline :: Integer -> IO ()
printline n = do
  let x = 10 ^ n
  putStr . show $ x
  putStr " -> "
  print $ result x 3 5

result :: Integer -> Integer -> Integer -> Integer
result x y z = sum factors where
  factors = filter factor35 numbers
  factor35 x = (isFactor y x) || (isFactor z x)
  isFactor x y = (mod y x) == 0
  numbers = [1..(x - 1)]
