{-# LANGUAGE BangPatterns #-}
module Yarn where

import           Control.Applicative
import           Control.Exception
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified System.IO                  as IO

import           Fiber


newtype Yarn = Yarn { unYarn :: IO.Handle }

data IOMode = ReadMode | ReadWriteMode

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
                        let fib = either (error . ("Yarn.foldl: " ++)) id $ Aeson.eitherDecode l
                        let x' = f x fib
                        go id second' x'
    finish front x = do
        let l = front BS.empty
        if BS.null l
            then return x
            else do
                let fib = either (error . ("Yarn.foldl: " ++)) id $ Aeson.eitherDecode l
                let x' = f x fib
                return x'
    !h = unYarn y

foldl' :: (a -> Fiber -> a) -> a -> Yarn -> IO a
foldl' f z y = do
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
                        let fib = either (error . ("Yarn.foldl: " ++)) id $ Aeson.eitherDecode l
                        let !x' = f x fib
                        go id second' x'
    finish front x = do
        let l = front BS.empty
        if BS.null l
            then return x
            else do
                let fib = either (error . ("Yarn.foldl: " ++)) id $ Aeson.eitherDecode l
                let !x' = f x fib
                return x'
    !h = unYarn y

insert :: Fiber -> Yarn -> IO ()
insert fib y = do
    IO.hSeek h IO.SeekFromEnd 0
    LBS.hPutStrLn h $ Aeson.encode fib
  where
    !h = unYarn y