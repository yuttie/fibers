{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Weave where

import           Control.Applicative
import           Data.Aeson                 (Value (..), object, (.=))
import qualified Data.Aeson.Parser          as Aeson
import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString            as BS
import           Data.Char
import qualified Data.HashMap.Strict        as HashMap
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Vector                as V

import           Fiber


-- constructor
-- can be used with Yarn.foldl
data Weaver a = Weaver (a -> a)

data Direction = AssignProp Text Value Direction
               | AppendElement Value

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
    -- A.char '['
    -- s <- jstring
    -- A.char ']'
    -- return s

pathComponents :: A.Parser [Text]
pathComponents = do
    !p <- topLevel
    ps <- A.many' (property <|> index)
    return $ p : ps

-- aaa
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

-- applyToJSON (Include path value) = foldr (\prop v -> object [prop .= v]) value props
--   where
--     Right props = A.parseOnly (pathComponents <* A.endOfInput) path

-- director :: A.Parser Director
-- director = do
--     root Root <$> topLevel
--     property <|> index

-- jsonWeaver :: Fiber -> Weaver Value
-- jsonWeaver fib = Weaber $ \v ->
--   where
--     path = case fib of
--         Equal path _ -> path
--         Include path _ -> path
