{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Anagrams where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Monad

{-

Functions you will need
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}

-- "/usr/share/dict/words"
-- Return all anagrams of the given string that appear in the given dictionary file.
-- anagrams :: Chars -> FilePath -> IO (List Chars)
-- anagrams s fp =
--   -- anagrams s fp = do
--   --   contents <- readFile fp
--   --   let dict = lines contents
--   --       perms = permutations s
--   --       matches = intersectBy (==) dict perms
--   --   pure matches
--   intersectBy equalIgnoringCase (permutations s) . lines <$> readFile fp

-- -- Compare two strings for equality, ignoring case
-- equalIgnoringCase :: Chars -> Chars -> Bool
-- equalIgnoringCase = on (==) (map toLower)
