{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<$>))
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           System.IO

import           Spinner


sourceFile :: FilePath -> Source
sourceFile fp = NeedIO $ do
    ls <- T.lines <$> T.readFile fp
    let sid = T.pack fp
    let src = foldr (\l next -> HaveOutput next $ Exist l "1") (fin sid) ls
    return $ ini src sid

main :: IO ()
main = do
    let src1 = sourceFile "test1.dat"
    let src2 = sourceFile "test2.dat"
    let src3 = sourceFile "test3.dat"
    let src = src1 `combine` src2 `combine` src3
    let sink = sinkHandle stdout
    spin src sink
