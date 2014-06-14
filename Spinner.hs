{-# LANGUAGE OverloadedStrings #-}
module Spinner where

import           Control.Applicative        ((<$>), (<*>))
import           Control.Monad              (liftM)
import           Control.Monad.IO.Class     (MonadIO (..), liftIO)
import           Data.Aeson                 (FromJSON (..), ToJSON (..), encode,
                                             object, (.:), (.=))
import qualified Data.Aeson.Types           as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           System.IO


type Key = Text
type Value = Text
data Fiber = Exist !Key !Value
type SourceID = Text

instance FromJSON Fiber where
    parseJSON (Aeson.Object v) = do
        typ <- v .: "type" :: Aeson.Parser Text
        case typ of
            "exist" -> Exist <$>
                       v .: ("key"::Text) <*>
                       v .: ("value"::Text)
            _ -> fail "parseJSON: unknown type"
    parseJSON _ = fail "parseJSON: failed to parse Fiber JSON"

instance ToJSON Fiber where
    toJSON (Exist key value) = object [ "type" .= ("exist" :: Text)
                                      , "key" .= key
                                      , "value" .= value ]

data Source m = Init (Source m) (Source m) SourceID
              | HaveOutput (Source m) Fiber
              | NeedIO (m (Source m))
              | Finishing (Source m) SourceID
              | Done

session :: Monad m => SourceID -> Source m -> Source m
session sid src = Init src Done sid `combine` Finishing Done sid

ini :: Source m -> Source m -> SourceID -> Source m
ini = Init

fin :: SourceID -> Source m
fin = Finishing Done

data Sink m = NeedInput (Fiber -> m (Sink m))
            | Closed

combine :: Monad m => Source m -> Source m -> Source m
combine (Init s1 s2 sid) s' = Init (s1 `combine` s') (s2 `combine` s') sid
combine (Finishing s sid) s' = Finishing (s `combine` s') sid
combine Done s' = s'
combine (NeedIO a) s' = NeedIO $ (`combine` s') `liftM` a
combine (HaveOutput s fib) s' = HaveOutput (s `combine` s') fib

sinkHandle :: MonadIO m => Handle -> (Sink m)
sinkHandle h = self
  where
    self = NeedInput $ \fib -> do
        liftIO $ BS.hPutStrLn h $ encode $ toJSON fib
        return self

dedup :: MonadIO m => Source m -> Source m
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
    go Done = Done

spin :: Monad m => Source m -> (Sink m) -> m ()
spin = go []
  where
    go stk (Init s _ sid) sink = go (sid : stk) s sink
    go [] (Finishing _ _) _ = fail "nothing to pop"
    go (sid:stk) (Finishing s sid') sink
      | sid /= sid' = fail "failed to pop"
      | otherwise = go stk s sink
    go stk Done _ = return ()
    go stk (NeedIO a) sink = do
        src <- a
        go stk src sink
    go stk (HaveOutput src fib) sink = case sink of
        Closed -> return ()
        NeedInput put -> do
            sink' <- put fib
            go stk src sink'
