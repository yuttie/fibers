{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS

import           Weave
import qualified Yarn


main :: IO ()
main = do
    y <- Yarn.openFile "test.yarn" Yarn.ReadMode
    r <- Yarn.foldl' (flip Weave.applyToJSON) (Aeson.object []) y
    LBS.writeFile "test.json" $ Aeson.encode r
