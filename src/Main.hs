module Main where

import           Application.CLI
import           Control.Monad                 (replicateM)
import           Text.ParserCombinators.Parsec

import           HooplaTelepay.Cruncher
import           HooplaTelepay.Parser.Telepay

data ValidateFile = ValidateFile
instance CLI ValidateFile where
  name _ = "summarize"
  desc _ = "parses and summarizes the telepay file"
  options _ = [ OptHelp [] (Just "String") "the input file." ]
  action _ _ = withStr "input-file" $ \filename -> execute $ validateFile filename


main :: IO ()
main = defaultMain $ with Help $ with ValidateFile $ initialize "Hoopla-telepay-cruncher 1.0"

validateFile :: FilePath -> IO ()
validateFile filename =
  do filecontents <- readFile filename
     case parse parseTelepay "Parsing Telepay file" filecontents of
       Left err -> error $ show err
       Right p  -> crunchTelepay p
