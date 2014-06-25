{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import           System.Environment   (getArgs)
import           System.IO            (stdout)

import           Weave
import qualified Yarn


main :: IO ()
main = do
    fp : _ <- getArgs
    Yarn.withFile fp Yarn.ReadMode $ \y -> do
        json <- Yarn.foldl' (flip Weave.applyToJSON) (Aeson.object []) y
        LBS.hPutStr stdout $ Aeson.encode json
