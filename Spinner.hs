{-# LANGUAGE OverloadedStrings #-}
module Spinner where

import           Control.Applicative          (Applicative (..), (<$>))
import           Control.Monad                (ap, liftM)
import           Control.Monad.IO.Class       (MonadIO (..), liftIO)
import           Control.Monad.Trans.Resource (MonadResource (..), allocate)
import           Data.Aeson                   (ToJSON (..), encode)
import qualified Data.ByteString.Lazy.Char8   as BS
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           System.IO

import           Fiber


type SourceID = Text

data Source m a = Init (Source m a) (Source m a) SourceID
                | HaveOutput (Source m a) Fiber
                | NeedIO (m (Source m a))
                | Finishing (Source m a) SourceID
                | Done a

instance Monad m => Functor (Source m) where
    fmap = liftM

instance Monad m => Applicative (Source m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Source m) where
    return = Done

    Init src1 src2 sid >>= f = Init (src1 >>= f) (src2 >>= f) sid
    HaveOutput src fib >>= f = HaveOutput (src >>= f) fib
    NeedIO msrc        >>= f = NeedIO ((>>= f) `liftM` msrc)
    Finishing src sid  >>= f = Finishing (src >>= f) sid
    Done x             >>= f = f x

session :: Monad m => SourceID -> Source m () -> Source m ()
session sid src = Init src (Done ()) sid >> Finishing (Done ()) sid

yield :: Fiber -> Source m ()
yield = HaveOutput (Done ())

dedup :: MonadIO m => Source m a -> Source m a
dedup = go
  where
    go (Init s1 s2 sid) = NeedIO $ do
        ls <- liftIO $ T.lines <$> T.readFile "known_sources.list"
        return $ if sid `elem` ls
            then go s2
            else Init (go s1) (error "impossible case") sid
    go (HaveOutput src o) = HaveOutput (go src) o
    go (NeedIO a) = NeedIO $ go `liftM` a
    go (Finishing src sid) = NeedIO $ do
        liftIO $ withFile "known_sources.list" AppendMode $ \h ->
            T.hPutStrLn h sid
        return $ Finishing (go src) sid
    go src@(Done _) = src

data Sink m = NeedInput (Fiber -> m (Sink m))
            | DoIO (m (Sink m))
            | Closed

sinkFile :: MonadResource m => FilePath -> Sink m
sinkFile fp = DoIO $ do
    (relKey, h) <- allocate (openFile fp WriteMode) hClose
    return $ go relKey h
  where
    go relKey h = NeedInput $ \fib -> do
        liftIO $ BS.hPutStrLn h $ encode $ toJSON fib
        return $ go relKey h

sinkHandle :: MonadIO m => Handle -> Sink m
sinkHandle h = self
  where
    self = NeedInput $ \fib -> do
        liftIO $ BS.hPutStrLn h $ encode $ toJSON fib
        return self

spin :: Monad m => Source m a -> Sink m -> m ()
spin = go []
  where
    go stk (Init s _ sid) sink = go (sid : stk) s sink
    go [] (Finishing _ _) _ = fail "nothing to pop"
    go (sid:stk) (Finishing s sid') sink
      | sid /= sid' = fail "failed to pop"
      | otherwise = go stk s sink
    go _ (Done _) _ = return ()
    go stk (NeedIO a) sink = do
        src <- a
        go stk src sink
    go stk src@(HaveOutput src' fib) sink = case sink of
        Closed -> return ()
        DoIO a -> do
            sink' <- a
            go stk src sink'
        NeedInput put -> do
            sink' <- put fib
            go stk src' sink'
