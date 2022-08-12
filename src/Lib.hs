{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where 

import Data.List
import Data.List.Split
import Data.Time.Format.ISO8601 
import Data.Time.Clock.POSIX 




posixToUTC :: Int -> String
posixToUTC x = iso8601Show (posixSecondsToUTCTime (realToFrac x))

formatInt :: Int -> [Char]
formatInt x = formatCommas (addCommas x)

formatString :: [Char] -> [Char]
formatString x = formatCommas (addCommas (readAsInteger x))

formatLovelace :: [Char] -> [Char]
formatLovelace x = formatCommas (addCommas (lovelaceToAda (readAsInteger x)))

formatMaybeToLovelace :: Maybe [Char] -> [Char]
formatMaybeToLovelace x = case x of
  Nothing  -> "N/A"   
  Just x -> (formatCommas (addCommas (lovelaceToAda (readAsInteger x))))

printMaybe :: Maybe [Char] -> [Char]
printMaybe x = case x of
  Nothing  -> "N/A"   
  Just x -> x

formatCommas :: [Char] -> [Char]
formatCommas x = formatted 
    where firstElement = Prelude.head x
          secondElement = [x !! 1]
          formatted
            | firstElement == ',' = Prelude.tail x
            | secondElement == [','] = Prelude.drop 2 x
            | otherwise = x

addCommas :: Show a => a -> [Char]
addCommas x = beforeDot ++ afterDot
    where
        splitWord = Data.List.break (== '.') $ show x
        beforeDot = Data.List.reverse (Data.List.intercalate "," $ Data.List.Split.chunksOf 3 $ Data.List.reverse $ fst splitWord)
        afterDot = snd splitWord

lovelaceToAda :: Integer -> Integer
lovelaceToAda x = x `div` 1000000

readAsInteger :: [Char] -> Integer
readAsInteger s = read s



