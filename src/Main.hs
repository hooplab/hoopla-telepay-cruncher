module Main where

import           Application.CLI
import           Text.ParserCombinators.Parsec
import Control.Monad (replicateM)

import HooplaTelapay.Parser.ApplicationHeader

data ValidateFile = ValidateFile
instance CLI ValidateFile where
  name _ = "validate"
  desc _ = "validates the given file"
  options _ = [ OptHelp [] (Just "String") "the input file, relative to the executable or absolute." ]
  action _ _ = withStr "input-file" $ \filename -> execute $ validateFile filename


main :: IO ()
main = defaultMain $ with Help $ with ValidateFile $ initialize "Hoopla-telepay-validator 1.0"

validateFile :: FilePath -> IO ()
validateFile filename =
  do filecontents <- readFile filename
     case parse validateTelepay "Validating Telepay file" filecontents of
       Left err -> error $ show err
       Right p  -> putStrLn "This is a valid telepay file!"


validateTelepay :: Parser String
validateTelepay =
  do validateBETFOR00

     validateApplicationHeader

     skipMany space



     readEnd
