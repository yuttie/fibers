module Spinner where

import           Control.Applicative ((<$>))
import           Data.Text           (Text)
import qualified Data.Text.IO        as T
import           System.IO


type Key = Text
type Value = Text
data Fiber = Exist !Key !Value
type SourceID = Text

data Source = Init Source SourceID
            | HaveOutput Source Fiber
            | NeedIO (IO Source)
            | Finishing Source SourceID
            | Done

session :: SourceID -> Source -> Source
session sid src = Init src sid `combine` Finishing Done sid

ini :: Source -> SourceID -> Source
ini = Init

fin :: SourceID -> Source
fin = Finishing Done

data Sink = NeedInput (Fiber -> IO Sink)
          | Closed

combine :: Source -> Source -> Source
combine (Init s sid) s' = Init (s `combine` s') sid
combine (Finishing s sid) s' = Finishing (s `combine` s') sid
combine Done s' = s'
combine (NeedIO a) s' = NeedIO $ (`combine` s') <$> a
combine (HaveOutput s fib) s' = HaveOutput (s `combine` s') fib

sinkHandle :: Handle -> Sink
sinkHandle h = self
  where
    self = NeedInput $ \fib -> case fib of
        Exist k v -> do
            T.hPutStr h k >> hPutChar h '\t' >> T.hPutStrLn h v
            return self

spin :: Source -> Sink -> IO ()
spin = go []
  where
    go stk (Init s sid) sink = do
        hPutStr stderr "begin \"" >> T.hPutStr stderr sid >> hPutStrLn stderr "\""
        go (sid : stk) s sink
    go [] (Finishing _ _) _ = fail "nothing to pop"
    go (sid:stk) (Finishing s sid') sink
      | sid /= sid' = fail "failed to pop"
      | otherwise = do
          hPutStr stderr "end \"" >> T.hPutStr stderr sid >> hPutStrLn stderr "\""
          go stk s sink
    go stk Done _ = return ()
    go stk (NeedIO a) sink = do
        src <- a
        go stk src sink
    go stk (HaveOutput src fib) sink = case sink of
        Closed -> return ()
        NeedInput put -> do
            sink' <- put fib
            go stk src sink'
