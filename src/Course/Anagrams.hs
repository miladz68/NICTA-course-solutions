{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Monad

import Course.Applicative
import Course.Traversable
import Course.Optional
{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
anagrams ls flName = do
  content <- readFile flName
  return $ intersectBy equalIgnoringCase (words content) (permutations ls)


-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase Nil Nil = True
equalIgnoringCase _ Nil = False
equalIgnoringCase Nil _ = False
equalIgnoringCase (a:.as) (b:.bs) = (toLower a == toLower b) && equalIgnoringCase as bs
