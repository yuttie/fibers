{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Weave where

import           Control.Applicative
import           Data.Aeson                 (Value (..), object)
import qualified Data.Aeson.Parser          as Aeson
import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.Attoparsec.Text       as A
import           Data.Char
import qualified Data.HashMap.Strict        as HashMap
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Vector                as V

import           Fiber


-- Parsers
identifier :: A.Parser Text
identifier = do
    ident <- A.scan True $ \firstChar c ->
        if firstChar
            then if isIdentInit c
                then Just False
                else Nothing
            else if isSubsequentChar c
                then Just firstChar
                else Nothing
    if T.null ident
        then fail "Weave.identifier: no identifier found"
        else return ident
  where
    isIdentInit c = isAlpha c || c == '_'
    isSubsequentChar c = isAlphaNum c || c == '_'

topLevel :: A.Parser Text
topLevel = identifier

property :: A.Parser Text
property = A.char '.' *> identifier

jstring :: A.Parser Text
jstring = do
    _ <- A.char '"'
    str <- A.scan False $ \s c ->
        if s
            then Just False
            else if c == '"'
                then Nothing
                else Just (c == '\\')
    _ <- A.char '"'
    let qstr = T.singleton '"' <> str <> T.singleton '"'
    let str' = either error id $ ABS.parseOnly Aeson.jstring $ T.encodeUtf8 qstr
    return $! str'

index :: A.Parser Text
index = A.char '[' *> jstring <* A.char ']'

pathComponents :: A.Parser [Text]
pathComponents = do
    !p <- topLevel
    ps <- A.many' (property <|> index)
    return $ p : ps

-- | Incremental construction of JSON value.
-- applyToJSON (include "abc[\"def\"].ghi" 2) $ applyToJSON (include "abc[\"def\"].ghi" 1) (object [])
-- => Object fromList [("abc",Object fromList [("def",Object fromList [("ghi",Array (fromList [Number 1.0,Number 2.0]))])])]
applyToJSON :: Fiber -> Value -> Value
applyToJSON (Equal path value) = go props value
  where
    props = either error id $ A.parseOnly (pathComponents <* A.endOfInput) path
    go [] v _ = v
    go (p:ps) v (Object o) = Object $ HashMap.insert p child' o
      where
        child = HashMap.lookupDefault Null p o
        child' = go ps v child
    go ps@(_:_) v _ = go ps v (object [])
applyToJSON (Include path value) = go props value
  where
    props = either error id $ A.parseOnly (pathComponents <* A.endOfInput) path
    go [] v (Array a) = Array $ V.snoc a v
    go [] v _ = Array $ V.singleton v
    go (p:ps) v (Object o) = Object $ HashMap.insert p child' o
      where
        child = HashMap.lookupDefault Null p o
        child' = go ps v child
    go ps@(_:_) v _ = go ps v (object [])
