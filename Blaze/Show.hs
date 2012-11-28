{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators, BangPatterns     #-}

module Blaze.Show (
  BlazeShow(..),
  showBS,
  showLBS,
  ) where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char.Utf8
import           Blaze.Text.Double
import           Blaze.Text.Int
import qualified Data.ByteString.Char8              as S
import qualified Data.ByteString.Lazy.Char8         as L
import           Data.List
import           Data.Monoid
import           GHC.Generics

class BlazeShow a where
  bshow :: a -> Builder

  default bshow :: (Generic a, GBlazeShow (Rep a)) => a -> Builder
  bshow = gbshow undefined . from
  {-# INLINE bshow #-}

showBS :: BlazeShow a => a -> S.ByteString
showBS = toByteString . bshow
{-# INLINE showBS #-}

showLBS :: BlazeShow a => a -> L.ByteString
showLBS = toLazyByteString . bshow
{-# INLINE showLBS #-}

class GBlazeShow f where
  gbshow :: (Builder -> Builder -> Builder) -> f a -> Builder

  isNull :: f a -> Bool
  isNull _ = False
  {-# INLINE isNull #-}

instance GBlazeShow U1 where
  gbshow _ U1 = mempty
  {-# INLINE gbshow #-}
  isNull _ = True
  {-# INLINE isNull #-}

instance (BlazeShow a) => GBlazeShow (K1 i a) where
  gbshow _ (K1 x) = bshow x
  {-# INLINE gbshow #-}

instance (GBlazeShow a, GBlazeShow b) => GBlazeShow (a :+: b) where
  gbshow f (L1 x) = gbshow f x
  gbshow f (R1 x) = gbshow f x
  {-# INLINE gbshow #-}

instance (GBlazeShow a, GBlazeShow b) => GBlazeShow (a :*: b) where
  gbshow f (x :*: y) =
    f (gbshow f x) (gbshow f y)
  {-# INLINE gbshow #-}

-- for 'C'onstructors
instance (GBlazeShow a, Constructor c) => GBlazeShow (M1 C c a) where
  gbshow _ c@(M1 x)
    | conIsRecord c =
      fromString cname <> fromString " {"
      <> gbshow (\a b -> a <> fromString ", " <> b) x
      <> fromChar '}'
    | conIsTuple =
      fromChar '(' <> gbshow (\a b -> a <> fromChar ',' <> b) x <> fromChar ')'
    | conFixity c == Prefix =
      fromString cname
       <> (if isNull x
             then mempty
             else fromChar ' ' <> gbshow (\a b -> a <> fromChar ' ' <> b) x)
    | otherwise = undefined
      -- FIXME: implement Infix
    where
      cname = conName c

      conIsTuple = case cname of
        ('(': ',': _) -> True
        _             -> False

-- for record 'S'electors
instance (GBlazeShow a, Selector s) => GBlazeShow (M1 S s a) where
  gbshow f s@(M1 x)
    | null sname = gbshow f x
    | otherwise =
      fromString (selName s)
      <> fromString " = "
      <> gbshow f x
    where
      sname = selName s
  {-# INLINE gbshow #-}

-- for 'D'ata types
instance (GBlazeShow a) => GBlazeShow (M1 D c a) where
  -- just ignore it
  gbshow f (M1 x) = gbshow f x
  {-# INLINE gbshow #-}

-- instances for basic generic types

instance BlazeShow Bool
instance BlazeShow Char   where bshow = fromChar
instance BlazeShow Double where bshow = double
instance BlazeShow Float  where bshow = float
instance BlazeShow Int    where bshow = integral
instance BlazeShow Ordering
instance BlazeShow ()

instance BlazeShow a => BlazeShow [a] where
  bshow l =
    fromChar '['
    <> mconcat (intersperse (fromChar ',') $ map bshow l)
    <> fromChar ']'
  {-# INLINE bshow #-}

instance BlazeShow a => BlazeShow (Maybe a)
instance (BlazeShow a, BlazeShow b) => BlazeShow (Either a b)

instance (BlazeShow a, BlazeShow b) => BlazeShow (a, b)
instance (BlazeShow a, BlazeShow b, BlazeShow c) => BlazeShow (a, b, c)
instance (BlazeShow a, BlazeShow b, BlazeShow c, BlazeShow d) => BlazeShow (a, b, c, d)
instance (BlazeShow a, BlazeShow b, BlazeShow c, BlazeShow d, BlazeShow e) => BlazeShow (a, b, c, d, e)
instance (BlazeShow a, BlazeShow b, BlazeShow c, BlazeShow d, BlazeShow e, BlazeShow f) => BlazeShow (a, b, c, d, e, f)
instance (BlazeShow a, BlazeShow b, BlazeShow c, BlazeShow d, BlazeShow e, BlazeShow f, BlazeShow g) => BlazeShow (a, b, c, d, e, f, g)
