module ParserSpec where

import System.IO
import Test.Hspec
import Parser

spec :: Spec
spec = describe "guavaParser" $ do
    it "should parse a program that is simply a module definition exposing functions" $
      parseFile "test/unit/programs/parsing/Module1.gv"
      `shouldReturn` 
      Module "Order" (Exposes []) (Statements [])
    
    it "should parse a program that is a module definition that does not expose anything" $
      parseFile "test/unit/programs/parsing/Module2.gv" 
      `shouldReturn`
      Module "Order" (Exposes ["create", "update", "delete"]) (Statements [])

    it "should parse a program with a function that returns a string literal with a single value inline type definition" $
      parseFile "test/unit/programs/parsing/Module3.gv"
      `shouldReturn` Module "Order" (Exposes ["create"])
        (Statements [Function $ FunctionWithInlineTypeDef "create" (Args []) [ValueTypeSingle "String"] (StringLiteral "931dd27e-5c78-4953-9742-52138fb9e9ba")])
   
    it "should parse a program with a function that returns a list of strings with a list value inline type definition" $
      parseFile "test/unit/programs/parsing/Module4.gv"
      `shouldReturn` Module "Order" (Exposes ["create"])
        (Statements [Function $ FunctionWithInlineTypeDef "create" (Args []) [ValueTypeList "String"] 
          (List [
            ListElemExpr (StringLiteral "931dd27e-5c78-4953-9742-52138fb9e9ba"), 
            ListElemExpr (StringLiteral "3870354d-0648-4bd5-ba01-1dd3cd44539a")
          ])])

    it "should parse a program with a function that returns an integer constant with a single value inline type definition" $
      parseFile "test/unit/programs/parsing/Module5.gv"
      `shouldReturn` Module "Order" (Exposes ["create"])
        (Statements [Function $ FunctionWithInlineTypeDef "create" (Args []) [ValueTypeSingle "Integer"]
          (IntegerConst 3043923)
        ])

    it "should parse a program with a function that returns a floating point constant with a single value inline type definition" $
      parseFile "test/unit/programs/parsing/Module6.gv"
      `shouldReturn` Module "Order" (Exposes ["total"])
        (Statements [Function $ FunctionWithInlineTypeDef "total" (Args []) [ValueTypeSingle "Float"] (FloatConst 540.34)])