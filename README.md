blaze-show
==========

An Alternative of Show class using blaze-builder with Generic Deriving

Examples
--------

BlazeShow class can derive automatically.

~~~ {.haskell}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.ByteString.Char8 as S
import           GHC.Generics
import           Blaze.Show

data Foo = Foo { fooA :: Int, fooB :: Bool }
  deriving (Show, Generics)

-- Derive automatically
instance BlazeShow Foo

main :: IO ()
main = do
  let v = Foo 123 True
  print v
  S.putStrLn $ showBS v
~~~
