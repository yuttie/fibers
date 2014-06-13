{-# LANGUAGE OverloadedStrings #-}
module Spinner where

import           Control.Applicative ((<$>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           System.IO


type Key = Text
type Value = Text
data Fiber = Exist !Key !Value

data Source = HaveOutput Source Fiber
            | NeedIO (IO Source)
            | Done

data Sink = NeedInput (Fiber -> IO Sink)
          | Closed

combine :: Source -> Source -> Source
combine Done s' = s'
combine (NeedIO a) s' = NeedIO $ (`combine` s') <$> a
combine (HaveOutput s fib) s' = HaveOutput (s `combine` s') fib

sourceFile :: FilePath -> Source
sourceFile fp = NeedIO $ do
    ls <- T.lines <$> T.readFile fp
    return $ foldr (\l src -> HaveOutput src $ Exist l "1") Done ls

sinkHandle :: Handle -> Sink
sinkHandle h = self
  where
    self = NeedInput $ \fib -> case fib of
        Exist k v -> do
            T.hPutStr h k >> hPutChar h '\t' >> T.hPutStrLn h v
            return self

spin :: Source -> Sink -> IO ()
spin = go
  where
    go Done _ = return ()
    go (NeedIO a) sink = do
        src <- a
        go src sink
    go (HaveOutput src fib) sink = case sink of
        Closed -> return ()
        NeedInput put -> do
            sink' <- put fib
            go src sink'

main :: IO ()
main = do
    let src = sourceFile "test_in.dat"
    let sink = sinkHandle stdout
    spin src sink
