module ParserSpec where

import System.IO
import Test.Hspec
import Parser

spec :: Spec
spec = describe "guavaParser" $ do
    it "should parse a program that is simply a module definition exposing functions" $
      parseFile "test/unit/programs/parsing/OrderModule1.gv"
      `shouldReturn` 
      Module "Order" (Exposes []) (Statements [])
    
    it "should parse a program that is a module definition that does not expose anything" $
      parseFile "test/unit/programs/parsing/OrderModule2.gv" 
      `shouldReturn`
      Module "Order" (Exposes ["create", "update", "delete"]) (Statements [])

    it "should parse a program with an empty function statement with an inline type definition reference" $
      parseFile "test/unit/programs/parsing/OrderModule3.gv"
      `shouldReturn` Module "Order" (Exposes ["create"])
        (Statements [Function $ FunctionWithInlineTypeDef "create" (Args []) [ValueTypeSingle "String"] (StringLiteral "Result")])