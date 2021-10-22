module Main where

isEmpty :: (Num a) => [a] -> Bool
isEmpty [] = True
isEmpty _ = False

filterUserInput :: String -> Integer
filterUserInput inputString =
  if integerValue > 5
    then defaultValue
    else integerValue
  where
    defaultValue = 3
    integerValue = read inputString :: Integer

getDigitsPower :: Integer -> Integer -> [Integer]
getDigitsPower number exponent = [getCharToNumber char ^ exponent | char <- stringNumber]
  where
    stringNumber = show number
    getCharToNumber :: Char -> Integer
    getCharToNumber character = read [character] :: Integer

getListOfNumbers :: Integer -> [Integer]
getListOfNumbers number =
  [curr | curr <- [rangeStart .. rangeEnd], sum (getDigitsPower curr number) == curr]
  where
    rangeStart = 10 ^ (number - 1)
    rangeEnd = (10 ^ number) - 1

main :: IO ()
main = do
  putStrLn "Enter natural numer that's smaller than 6:"
  rawInput <- getLine
  let filteredInput = filterUserInput rawInput
  let results = getListOfNumbers filteredInput
  let resultsSum = sum results
  print resultsSum
