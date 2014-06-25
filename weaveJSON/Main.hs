{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Options.Applicative
import           System.IO            (IOMode (..), hClose, openFile, stdout)

import           Weave
import qualified Yarn


data CommandLine = CommandLine
    { clOutputFp :: !(Maybe FilePath)
    , clInputFp  :: !(Maybe FilePath)
    }

cmdline :: Parser CommandLine
cmdline = CommandLine
    <$> optOutputFp
    <*> argInputFp
  where
    optOutputFp = fmap join $ optional $ nullOption
                ( long "output"
               <> short 'o'
               <> metavar "FILE"
               <> reader parseFilePath
                )
    argInputFp = fmap join $ optional $ argument parseFilePath
               ( metavar "FILE" )

parseFilePath :: Monad m => String -> m (Maybe FilePath)
parseFilePath "-" = return Nothing
parseFilePath s = return $! Just s

main :: IO ()
main = do
    cl <- execParser $ info (helper <*> cmdline) fullDesc
    y <- case clInputFp cl of
        Nothing -> return Yarn.stdin
        Just fp -> Yarn.openFile fp Yarn.ReadMode
    h <- case clOutputFp cl of
        Nothing -> return stdout
        Just fp -> openFile fp WriteMode
    json <- Yarn.foldl' (flip Weave.applyToJSON) (Aeson.object []) y
    Yarn.close y
    LBS.hPutStr h $ Aeson.encode json
    hClose h
