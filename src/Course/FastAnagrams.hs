{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

import Course.Monad

import Course.Applicative
import Course.Traversable
import Course.Optional

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams ls flName = do
  content <- readFile flName
  let dicSet = S.fromList (hlist (words content))
  return $ foldLeft (\acc x -> if x `S.member` dicSet then x:.acc else acc) Nil (permutations ls)

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
