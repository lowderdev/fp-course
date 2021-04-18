{-# LANGUAGE ScopedTypeVariables #-}

-- {-# LANGUAGE NoImplicitPrelude #-}

module Course.FastAnagrams where

-- import Course.Applicative
-- import Course.Core
-- import Course.Functor
-- import Course.List
-- import Course.Monad

import Data.Char
import Data.Function
import Data.List
import qualified Data.Set as S
import Data.String
import Prelude

anagrams :: String -> FilePath -> IO [String]
anagrams s fp =
  intersectBy equalIgnoringCase (permutations s) . lines <$> readFile fp

-- Compare two strings for equality, ignoring case
equalIgnoringCase :: String -> String -> Bool
equalIgnoringCase = on (==) (map toLower)

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"
fastAnagrams :: String -> String -> IO [String]
fastAnagrams s fp = do
  contents <- readFile fp
  let dict = S.fromList $ lines contents
      perms = S.fromList $ permutations s
      matches = S.intersection dict perms
  pure . S.toList $ matches

-- suggestion from timjput
otherAnagrams :: String -> String -> IO [String]
otherAnagrams s fp = do
  contents <- readFile fp
  let dict = lines contents
      sortedS = sort s
      matches = filter ((sortedS `equalIgnoringCase`) . sort) dict
  pure matches

newtype NoCaseString = NoCaseString {ncString :: String}

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Ord NoCaseString where
  compare = compare `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
