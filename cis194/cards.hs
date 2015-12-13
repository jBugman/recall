module CardValidation where

-- |
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- |
-- >>> toDigitsRev 1234
-- [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0    = []
  | x < 10    = [x]
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

-- |
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse
  where
    doubleEveryOther' [] = []
    doubleEveryOther' [x] = [x]
    doubleEveryOther' (x:y:xs) = x : y * 2 : doubleEveryOther' xs

-- |
-- >>> sumDigits [16,7,12,5]
-- 22
-- >>> sumDigits [1, 2, 3]
-- 6
sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigits) 0

-- |
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False
validate :: Integer -> Bool
validate x = checksum `mod` 10 == 0
  where
    checksum = sumDigits $ doubleEveryOther $ toDigits x
