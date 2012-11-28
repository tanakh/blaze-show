{-# LANGUAGE DeriveGeneric #-}

import Criterion.Main
import GHC.Generics

import Blaze.Show

data Foo a = Foo { fooA :: a, fooB :: Bool } deriving (Show, Generic)
instance BlazeShow a => BlazeShow (Foo a)

benchAll :: (Show a, BlazeShow a) => a -> [Benchmark]
benchAll x =
  [ bench "show"    $ nf show    x
  , bench "showBS"  $ nf showBS  x
  , bench "showLBS" $ nf showLBS x
  ]

main :: IO ()
main = do
   defaultMain
     [ bgroup "Bool" $ benchAll False
     , bgroup "Int"  $ benchAll (1234567890 :: Int)
     , bgroup "UserDef" $ benchAll (Foo (Foo (123 :: Int) True) True)
     , bgroup "List" $ benchAll ([1..1000] :: [Int])
     ]
