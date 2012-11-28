{-# LANGUAGE DeriveGeneric #-}

import           Criterion.Main
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           GHC.Generics

import           Blaze.Show

data Foo a = Foo { fooA :: a, fooB :: Bool } deriving (Show, Generic)
instance BlazeShow a => BlazeShow (Foo a)

benchAll :: (Show a, BlazeShow a) => a -> [Benchmark]
benchAll x =
  [ bench "show"        $ nf show    x
  , bench "S.pack.show" $ nf (S.pack . show) x
  , bench "L.pack.show" $ nf (L.pack . show) x
  , bench "showBS"      $ nf showBS  x
  , bench "showLBS"     $ nf showLBS x
  ]

main :: IO ()
main = do
   defaultMain
     [ bgroup "Bool" $ benchAll False
     , bgroup "Int"  $ benchAll (1234567890 :: Int)
     , bgroup "UserDef" $ benchAll (Foo (Foo (123 :: Int) True) True)
     , let ls = [1..10000::Int] in bgroup "List" $ benchAll ls
     ]
