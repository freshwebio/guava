{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import System.IO
import System.Directory
import Control.Monad
import Text.Printf
import Test.Hspec
import Data.Aeson
import Data.Either
import Data.String.Utils
import qualified Data.ByteString.Lazy as LB

import JSONSpecAST

import Parser

specsFolder = "test/unit/programs/parsing/expected/"
programsFolder = "test/unit/programs/parsing/"

loadFixtures :: IO [(String, Either String SpecAST)]
loadFixtures = do
  dirContents <- getDirectoryContents specsFolder
  specFileNames <- filterM (fmap not . doesDirectoryExist) dirContents
  specStrings <- mapM (LB.readFile . (++) specsFolder) specFileNames
  let specs = map eitherDecode specStrings
  return $ zip (map (replace ".json" ".gv" . (programsFolder ++)) specFileNames) specs

spec :: Spec
spec = describe "guavaParser" $ do
       specs <- runIO loadFixtures
       forM_ specs $ \(inputFileName, specAST) ->
         case specAST of
          Left err -> fail (err ++ " in module spec " ++ inputFileName)
          Right wrappedSpec -> it (description wrappedSpec) $
            parseFile inputFileName `shouldReturn` moduleAST wrappedSpec