module ParserSpec where

import System.IO
import Test.Hspec
import Parser

spec :: Spec
spec = do
  describe "parseFile" $ do
    it "should parse a program that is simply a module definition exposing functions" $ do
      parseString "module Order exposing { create, update }" `shouldBe` (ModuleExposing "Order" (Exposes ["create", "update"]) (Statements []))
    
    it "should parse a program that is a module definition that does not expose anything" $ do
      parseString "module Order" `shouldBe` (Module "Order" (Statements []))