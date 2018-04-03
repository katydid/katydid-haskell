-- |
-- This is an internal relapse module.
--
-- It zips patterns to reduce the state space.

module Zip (
    Zipper, zippy, unzipby
) where

import qualified Data.Set as S
import Data.List (elemIndex)

import Patterns

data ZipEntry = ZipVal Int | ZipZAny | ZipNotZAny
    deriving (Eq, Ord)

-- |
-- Zipper represents compressed indexes
-- that resulted from compressing a list of patterns.
-- This can be used to uncompress a list of bools (nullability of patterns).
newtype Zipper = Zipper [ZipEntry]
    deriving (Eq, Ord)

-- | zippy compresses a list of patterns.
zippy :: [Pattern] -> ([Pattern], Zipper)
zippy ps =
    let s = S.fromList ps
        s' = S.delete ZAny s
        s'' = S.delete (Not ZAny) s'
        l = S.toAscList s''
    in (l, Zipper $ map (indexOf l) ps)

indexOf :: [Pattern] -> Pattern -> ZipEntry
indexOf _ ZAny = ZipZAny
indexOf _ (Not ZAny) = ZipNotZAny
indexOf ps p = case elemIndex p ps of
    (Just i) -> ZipVal i

-- | unzipby uncompresses a list of bools (nullability of patterns).
unzipby :: Zipper -> [Bool] -> [Bool]
unzipby (Zipper z) bs = map (ofIndexb bs) z

ofIndexb :: [Bool] -> ZipEntry -> Bool
ofIndexb _ ZipZAny = True
ofIndexb _ ZipNotZAny = False
ofIndexb bs (ZipVal i) = bs !! i