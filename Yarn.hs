{-# LANGUAGE BangPatterns #-}
module Yarn
    ( Yarn (..)
    , IOMode (..)
    , stdin
    , stdout
    , withFile
    , openFile
    , close
    , foldl
    , foldl'
    , insert
    ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Prelude                    hiding (foldl)
import qualified System.IO                  as IO

import           Fiber


newtype Yarn = Yarn { unYarn :: IO.Handle }

data IOMode = ReadMode | ReadWriteMode

stdin :: Yarn
stdin = Yarn IO.stdin

stdout :: Yarn
stdout = Yarn IO.stdout

withFile :: FilePath -> IOMode -> (Yarn -> IO a) -> IO a
withFile fp mode = bracket (openFile fp mode) close

openFile :: FilePath -> IOMode -> IO Yarn
openFile fp mode = Yarn <$> IO.openFile fp modeIO
  where
    modeIO = case mode of
        ReadMode -> IO.ReadMode
        ReadWriteMode -> IO.ReadWriteMode

close :: Yarn -> IO ()
close = IO.hClose . unYarn

foldl :: (a -> Fiber -> a) -> a -> Yarn -> IO a
foldl f z y = do
    seekable <- IO.hIsSeekable $ unYarn y
    when seekable $
        IO.hSeek h IO.AbsoluteSeek 0
    loop id z
  where
    loop front x = do  -- more bytes are needed
        eof <- IO.hIsEOF h
        if eof
            then finish front x
            else do
                buf <- BS.hGet h 4096
                go front buf x
    go front more x = do  -- 'more' is always the beginning of a line
        let (first, second) = BS.break (== '\n') more
        case BS.uncons second of
            Nothing -> loop (BS.append $ front more) x
            Just (_, second') -> do
                let l = front first
                if BS.null l
                    then fail "Yarn.foldl: an empty line"
                    else do
                        let fib = either (error . ("Yarn.foldl: " ++)) id $ Aeson.eitherDecode $ LBS.fromStrict l
                        let x' = f x fib
                        go id second' x'
    finish front x = do
        let l = front BS.empty
        if BS.null l
            then return x
            else do
                let fib = either (error . ("Yarn.foldl: " ++)) id $ Aeson.eitherDecode $ LBS.fromStrict l
                let x' = f x fib
                return x'
    !h = unYarn y

foldl' :: (a -> Fiber -> a) -> a -> Yarn -> IO a
foldl' f z y = do
    seekable <- IO.hIsSeekable $ unYarn y
    when seekable $
        IO.hSeek h IO.AbsoluteSeek 0
    loop id z
  where
    loop front x = do  -- more bytes are needed
        eof <- IO.hIsEOF h
        if eof
            then finish front x
            else do
                buf <- BS.hGet h 4096
                go front buf x
    go front more x = do  -- 'more' is always the beginning of a line
        let (first, second) = BS.break (== '\n') more
        case BS.uncons second of
            Nothing -> loop (BS.append $ front more) x
            Just (_, second') -> do
                let l = front first
                if BS.null l
                    then fail "Yarn.foldl: an empty line"
                    else do
                        let fib = either (error . ("Yarn.foldl: " ++)) id $ Aeson.eitherDecode $ LBS.fromStrict l
                        let !x' = f x fib
                        go id second' x'
    finish front x = do
        let l = front BS.empty
        if BS.null l
            then return x
            else do
                let fib = either (error . ("Yarn.foldl: " ++)) id $ Aeson.eitherDecode $ LBS.fromStrict l
                let !x' = f x fib
                return x'
    !h = unYarn y

insert :: Fiber -> Yarn -> IO ()
insert fib y = do
    seekable <- IO.hIsSeekable $ unYarn y
    when seekable $
        IO.hSeek h IO.SeekFromEnd 0
    LBS.hPutStrLn h $ Aeson.encode fib
  where
    !h = unYarn y
