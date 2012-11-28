{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.ByteString.Char8 as S
import           GHC.Generics

import           Blaze.Show

data Foo
  = Foo1 { foo1A :: Int, foo1B :: Int }
  | Foo2 { foo2A :: Int, foo2B :: Int, foo2C :: Int }
  | Foo3 Int Int
  deriving (Show, Generic)

instance BlazeShow Foo

data Bar = Bar { barA :: Foo } deriving (Show, Generic)

instance BlazeShow Bar

print2 :: (Show a, BlazeShow a) => a -> IO ()
print2 v = do
  putStrLn $ show v
  S.putStrLn $ showBS v

main :: IO ()
main = do
  print2 ()
  print2 (1 :: Int, 2 :: Int)
  print2 [1::Int, 2, 3]
  print2 (Just 1  :: Maybe Int)
  print2 (Nothing :: Maybe Int)
  print2 (Right 1 :: Either Int Int)
  print2 (Left 1  :: Either Int Int)
  print2 (Foo1 123 456)
  print2 (Foo2 123 456 789)
  print2 (Foo3 123 456)
  print2 (Bar $ Foo1 123 456)
  print2 (True, False)
