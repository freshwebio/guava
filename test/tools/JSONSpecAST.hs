{-# LANGUAGE OverloadedStrings #-}

module JSONSpecAST where

import Data.Aeson

import Parser

data SpecAST = SpecAST { description :: String, moduleAST :: Module } deriving Show

instance FromJSON FunctionImpl where
  parseJSON = withObject "FunctionImpl" $ \o -> do
    functionImplName <- o .: "functionImplName"
    functionDefArgs <- o .: "functionDefArgs"
    functionImplExpression <- o .: "functionImplExpression"
    return $ FunctionImpl functionImplName functionDefArgs functionImplExpression

instance FromJSON ListElementExpr where
  parseJSON = withObject "ListElementExpr" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "listElemSpread" -> ListElemSpread <$> o .: "spreadExpression"
      "listElemExpr" -> ListElemExpr <$> o .: "listElemExpression"
      _ -> fail("unknown list element expression kind: " ++ kind)

instance FromJSON LetVarBind where
  parseJSON = withObject "LeftVarBind" $ \o -> do
    varBindName <- o .: "varBindName"
    varBindExpression <- o .: "varBindExpression"
    return $ LetVarBind varBindName varBindExpression

instance FromJSON ABinOp where
  parseJSON = withObject "ABinOp" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "add" -> return Add
      "subtract" -> return Subtract
      "multiply" -> return Multiply
      "divide" -> return Divide
      _ -> fail("unknown arithmetic binary operator kind: " ++ kind)

instance FromJSON AExpr where
  parseJSON = withObject "AExpr" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "integerConst" -> IntegerConst <$> o .: "intConstValue"
      "floatConst" -> FloatConst <$> o .: "floatConstValue"
      "numericVar" -> NumericVar <$> o .: "numericVarName"
      "neg" -> Neg <$> o .: "negativeArithmeticExpression"
      "arithmeticFunctionCall" -> ArithmeticFunctionCall <$> o .: "calledArithmeticFunctionName" 
                                                         <*> o .: "arithmeticFuncCallArgs"
      "aBinary" -> ABinary <$> o .: "arithmeticBinaryOperator" 
                           <*> o .: "arithmeticExpressionLeft" 
                           <*> o .: "arithmeticExpressionRight"
      _ -> fail ("unknown arithmetic expression kind: " ++ kind)

instance FromJSON CallArguments where
  parseJSON = withObject "CallArguments" $ \o -> do
    callArgs <- o .: "callArgs"
    return $ CallArgs callArgs

instance FromJSON BBinOp where
  parseJSON = withObject "BBinOp" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "and" -> return And
      "or" -> return Or
      _ -> fail ("unknown boolean binary operator kind: " ++ kind)

instance FromJSON BExpr where
  parseJSON = withObject "BExpr" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "boolConst" -> BoolConst <$> o .: "boolConst"
      "boolVar" -> BoolVar <$> o .: "boolVarName"
      "not" -> Not <$> o .: "negateBoolExpression"
      "bBinary" -> BBinary <$> o .: "boolBinaryOperator" 
                           <*> o .: "boolExpressionLeft" 
                           <*> o .: "boolExpressionRight"
      "boolFunctionCall" -> BoolFunctionCall <$> o .: "calledBoolFunctionName" <*> o .: "boolFuncCallArgs"
      _ -> fail ("unknown boolean expression kind: " ++ kind)

instance FromJSON CaseMatch where
  parseJSON = withObject "CaseMatch" $ \o -> do
    caseMatchPattern <- o .: "caseMatchPattern"
    caseMatchExpr <- o .: "caseMatchExpression"
    return $ Case caseMatchPattern caseMatchExpr

instance FromJSON CaseMatches where
  parseJSON = withObject "CaseMatches" $ \o -> do
    caseMatchList <- o .: "caseMatchList"
    return $ CaseMatchSeq caseMatchList

instance FromJSON Expr where
  parseJSON = withObject "Expr" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "stringLiteral" -> StringLiteral <$> o .: "stringLiteralValue"
      "var" -> Var <$> o .: "varName"
      "caseBlock" -> CaseBlock <$> o .: "caseCondExpr" <*> o .: "caseMatchSeq"
      "if" -> If <$> o .: "boolExpr" <*> o .: "ifThen" <*> o .: "ifElse"
      "boolExpr" -> BoolExpr <$> o .: "boolExpr"
      "arithmeticExpr" -> ArithmeticExpr <$> o .: "arithmeticExpr"
      "letInExpr" -> LetInExpr <$> o .: "letVarBindings" <*> o .: "letInUsage"
      "list" -> List <$> o .: "listElements"
      "functionCall" -> FunctionCall <$> o .: "calledFunctionName" <*> o .: "functionCallArgs"
      _ -> fail ("unknown expression kind: " ++ kind)

instance FromJSON TypeDef where
  parseJSON = withObject "TypeDef" $ \o -> do
    name <- o .: "typeDefName"
    valueTypes <- o .: "typeDefValueTypes"
    return $ TypeDef name valueTypes

instance FromJSON ValueType where
  parseJSON = withObject "ValueType" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "valueTypeSingle" -> ValueTypeSingle <$> o .: "valueTypeName"
      "valueTypeList" -> ValueTypeList <$> o .: "valueTypeName"
      "valueTypeFunction" -> ValueTypeFunction <$> o .: "functionValueTypes"
      _ -> fail ("unknown value type kind: " ++ kind)

instance FromJSON Pattern where
  parseJSON = withObject "Pattern" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "patternBindingEmptyList" -> return PatternBindingEmptyList
      "patternBindingListDestructureNElements" -> 
        PatternBindingListDestructureNElements <$> o .: "destructuredElements"
      "patternBindingListDestructure" ->
        PatternBindingListDestructure <$> o .: "destructuredElements" <*> o .: "tail"
      _ -> fail ("unknown pattern argument kind; " ++ kind)

instance FromJSON DefArgument where
  parseJSON = withObject "DefArgument" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "defArgName" -> DefArgName <$> o .: "argName"
      "defArgPattern" -> DefArgPattern <$> o .: "argPattern"
      _ -> fail ("unknown function definition argument kind: " ++ kind)

instance FromJSON DefArguments where
  parseJSON = withObject "DefArguments" $ \o -> do
    args <- o .: "args"
    return $ Args args

instance FromJSON FunctionStmt where
  parseJSON = withObject "FunctionStmt" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "functionWithInlineTypeDef" ->
        FunctionWithInlineTypeDef <$> o .: "functionName"
                                  <*> o .: "defArgs"
                                  <*> o .: "functionTypeDefValueTypes"
                                  <*> o .: "functionExpr"
      "functionMultipleInstancesTypeDef" ->
        FunctionMultipleInstancesTypeDef <$> o .: "functionName"
                                         <*> o .: "functionTypeDef"
                                         <*> o .: "functionImpls"
      "functionMultipleInstancesTypeDefRef" ->
        FunctionMultipleInstancesTypeDefRef <$> o .: "functionName"
                                            <*> o .: "functionTypeDefRef"
                                            <*> o .: "functionImpls"
      "functionWithTypeDefRef" ->
        FunctionWithTypeDefRef <$> o .: "functionName"
                               <*> o .: "defArgs"
                               <*> o .: "functionTypeDefRef"
                               <*> o .: "functionExpr"
      _ -> fail ("unknown function statement kind: " ++ kind)

instance FromJSON Stmt where
  parseJSON = withObject "Function or TypeDefinition" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "function" -> Function <$> o .: "fncStatement"
      "typeDefinition" -> TypeDefinition <$> o .: "typeDef"
      _ -> fail ("unknown statement kind: " ++ kind)

instance FromJSON Stmts where
  parseJSON = withObject "Stmts" $ \o -> do
    statementList <- o .: "statementList"
    return $ Statements statementList

instance FromJSON Exposing where
  parseJSON = withObject "Exposing" $ \o -> do
    exposed <- o .: "exposed"
    return $ Exposes exposed

instance FromJSON Module where
  parseJSON = withObject "Module" $ \o -> do
    name <- o .: "name"
    exposes <- o .: "exposes"
    moduleStatements <- o .: "moduleStatements"
    return $ Module name exposes moduleStatements

instance FromJSON SpecAST where
  parseJSON = withObject "SpecAST" $ \o -> do
    description <- o .: "description"
    moduleAST <- o .: "module"
    return $ SpecAST description moduleAST