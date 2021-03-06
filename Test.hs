{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (MonadResource (..), allocate,
                                               release, runResourceT)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           System.IO

import           Fiber
import           Spin
import           Weave


sourceFile :: MonadResource m => FilePath -> Source m ()
sourceFile fp = NeedIO $ do
    (relKey, h) <- allocate (openFile fp ReadMode) hClose
    return $ Init (go relKey h) (Done ()) sid
  where
    sid = T.pack fp
    go relKey h = NeedIO $ do
        eof <- liftIO $ hIsEOF h
        if eof
            then do
                release relKey
                return $ Finishing (Done ()) sid
            else do
                l <- liftIO $ T.hGetLine h
                return $ HaveOutput (go relKey h) (Equal l "1")

main :: IO ()
main = do
    let src1 = sourceFile "test1.dat"
    let src2 = sourceFile "test2.dat"
    let src3 = sourceFile "test3.dat"
    let src4 = sourceFile "test4.dat"
    let src5 = sourceFile "test5.dat"
    let srcs1 = dedup $ src1 >> src1 >> src2 >> src3 >> src2
    let srcs2 = dedup $ src1 >> src4 >> src4 >> src3 >> src5
    let sink1 = sinkHandle stdout
    let sink2 = sinkFile "test.yarn"
    runResourceT $ spin srcs1 sink1
    runResourceT $ spin srcs2 sink2
    yield (equal "fibers.test.x" (1::Int)) `spin` sinkHandle stdout
