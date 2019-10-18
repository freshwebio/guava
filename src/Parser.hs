module Parser where

import Control.Monad
import Text.Parsec.Expr
import Text.Parsec
import qualified Text.Parsec.Token as Token
import Lexer

data Module = Module { name :: String, exposes :: Exposing, moduleStatements :: Stmts } deriving (Show, Eq)

newtype Exposing = Exposes { exposed :: [String] } deriving (Show, Eq)

newtype Stmts = Statements { statementList :: [Stmt] } deriving (Show, Eq)

data Stmt = Function { fncStatement :: FunctionStmt }
          | TypeDefinition { typeDef :: TypeDef }
          deriving (Show, Eq)

data FunctionStmt = FunctionWithInlineTypeDef { functionName :: String, defArgs :: DefArguments, functionTypeDefValueTypes :: [ValueType], functionExpr :: Expr }
                  | FunctionMultipleInstancesTypeDef { functionName :: String, functionTypeDef :: TypeDef,  functionImpls :: [FunctionImpl] }
                  | FunctionMultipleInstancesTypeDefRef { functionName :: String, functionTypeDefRef :: String, functionImpls :: [FunctionImpl] }
                  | FunctionWithTypeDefRef { functionName :: String, defArgs :: DefArguments, functionTypeDefRef :: String, functionExpr :: Expr }
                  deriving (Show, Eq)

data FunctionImpl = FunctionImpl { functionImplName :: String, functionDefArgs :: DefArguments, functionImplExpression :: Expr } deriving (Show, Eq)

data TypeDef = TypeDef { typeDefName :: String, typeDefValueTypes :: [ValueType] } deriving (Show, Eq)

data ValueType = ValueTypeSingle { valueTypeName :: String } |
                 ValueTypeList { valueTypeName :: String } |
                 ValueTypeFunction { functionValueTypes :: [ValueType] } deriving (Show, Eq)

newtype DefArguments = Args { args :: [DefArgument] } deriving (Show, Eq)

data DefArgument = DefArgName { argName :: String } 
                 | DefArgPattern { argPattern :: Pattern } deriving (Show, Eq)

data Expr = StringLiteral { stringLiteralValue :: String }
          | Var { varName :: String }
          | CaseBlock { caseCondExpr :: Expr, caseMatchSeq :: CaseMatches }
          | If { boolExpr :: BExpr, ifThen :: Expr, ifElse :: Expr }
          | BoolExpr { boolExpr :: BExpr }
          | ArithmeticExpr { arithmeticExpr :: AExpr }
          | LetInExpr { letVarBindings :: [LetVarBind], letInUsage :: Expr }
          | List { listElements :: [ListElementExpr] }
          | FunctionCall { calledFunctionName :: String, functionCallArgs :: CallArguments }
          deriving (Show, Eq)

data ListElementExpr = ListElemSpread { spreadExpression :: Expr }
                     | ListElemExpr { listElemExpression :: Expr }
                     deriving (Show, Eq)

data LetVarBind = LetVarBind { varBindName :: String, varBindExpression :: Expr } deriving (Show, Eq)

data BExpr = BoolConst { boolConstValue :: Bool }
           | BoolVar { boolVarName :: String }
           | Not { negateBoolExpression :: BExpr }
           | BBinary { boolBinaryOperator :: BBinOp, boolExpressionLeft :: BExpr, boolExpressionRight :: BExpr }
           | BoolFunctionCall { calledBoolFunctionName :: String, boolFuncCallArgs :: CallArguments }
           deriving (Show, Eq)

data AExpr = IntegerConst { intConstValue :: Integer }
           | FloatConst { floatConstValue :: Double }
           | NumericVar { numericVarName :: String }
           | Neg { negativeArithmeticExpression :: AExpr }
           | ArithmeticFunctionCall { calledArithmeticFunctionName :: String, arithmeticFuncCallArgs :: CallArguments }
           | ABinary { arithmeticBinaryOperator :: ABinOp,  arithmeticExpressionLeft :: AExpr, arithmeticExpressionRight :: AExpr }
           deriving (Show, Eq)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
            deriving (Show, Eq)

newtype CallArguments = CallArgs { callArgs :: [Expr] } deriving (Show, Eq)

data BBinOp = And | Or deriving (Show, Eq)

newtype CaseMatches = CaseMatchSeq { caseMatchList :: [CaseMatch] } deriving (Show, Eq)

data CaseMatch = Case { caseMatchPattern :: Pattern, caseMatchExpression :: Expr } deriving (Show, Eq)

data Pattern = PatternBindingEmptyList -- []
             | PatternBindingListDestructureNElements { destructuredElements :: [String] } -- (x:[] x:y:[] x:y:z:[])
             | PatternBindingListDestructure { destructuredElements :: [String], tail :: String } -- (x:xs x:y:ys x:y:z:zs)
             deriving (Show, Eq)

guavaParser = whiteSpace >> guavaModule

guavaModule =
  do reserved "module"
     name <- identifier
     exposes <- exposing
     Module name exposes <$> statements

exposing = (
  do reserved "exposing"
     Exposes <$> braces (sepBy1 identifier comma)
  ) <|> return (Exposes [])

statements = (eof >> return (Statements []))
           <|> Statements <$> sepEndBy statement endOfLine

statement = Function <$> functionStatement
          <|> TypeDefinition <$> typeDefinition

functionStatement = 
   do reserved "function"
      name <- identifier
      functionStatementBody name

functionStatementBody name = functionWithInlineTypeDef name
                  <|> functionMultipleInstancesTypeDef name
                  <|> functionMultipleInstancesTypeDefRef name
                  <|> functionWithTypeDefRef name

functionWithInlineTypeDef name = 
  do defArgs <- functionDefinitionArguments
     symbol "::"
     vTypes <- valueTypes
     FunctionWithInlineTypeDef name defArgs vTypes <$> braces expression

functionWithTypeDefRef name =
  do defArgs <- functionDefinitionArguments
     symbol "::"
     typeDefRef <- identifier
     FunctionWithTypeDefRef name defArgs typeDefRef <$> expression

functionMultipleInstancesTypeDef name =
  do symbol "::"
     typeDef <- typeDefinition
     fncImpls <- functionImplementations
     FunctionMultipleInstancesTypeDef name typeDef <$> functionImplementations
  
functionMultipleInstancesTypeDefRef name =
  do typeDefRef <- identifier
     FunctionMultipleInstancesTypeDefRef name typeDefRef <$> functionImplementations

functionImplementations = sepBy functionImplementation endOfLine

functionImplementation =
  do name <- identifier
     defArgs <- functionDefinitionArguments
     FunctionImpl name defArgs <$> expression

functionDefinitionArguments = try (parens functionDefinitionRegularArgs)
  <|> (
     do symbol "()"
        return $ Args []
  )

functionDefinitionRegularArgs = Args <$> sepBy1 functionDefinitionArgument comma

functionDefinitionArgument = DefArgName <$> identifier
                           <|> DefArgPattern <$> parsePattern

parsePattern = patternBindingEmptyList
             <|> patternBindingListDestructureNElements
             <|> patternBindingListDestructure

patternBindingEmptyList =
  do symbol "[]"
     return PatternBindingEmptyList

patternBindingListDestructureNElements =
  do destructuredElements <- sepEndBy1 identifier $ symbol ":"
     symbol "[]"
     return $ PatternBindingListDestructureNElements destructuredElements

patternBindingListDestructure =
  do destructuredElements <- sepBy1 identifier $ symbol ":"
     return $ PatternBindingListDestructure (init destructuredElements) (last destructuredElements)

typeDefinition =
  do reserved "type"
     typeName <- identifier
     symbol "::"
     TypeDef typeName <$> valueTypes

valueTypes =
   do first <- valueType
      valueTypesList first

valueTypesList first = (
   do symbol "->"
      rest <- sepBy1 valueType (symbol "->")
      return $ first:rest
   ) <|> return [first]

valueType = valueTypeFunction
          <|> valueTypeSingleOrList

valueTypeSingleOrList =
   do typeIdent <- identifier
      completeValueTypeSingleOrList typeIdent

completeValueTypeSingleOrList typeIdent = valueTypeList typeIdent
                              <|> return (ValueTypeSingle typeIdent)

valueTypeList typeIdent =
  do symbol "[]"
     return $ ValueTypeList typeIdent

valueTypeFunction =
  do valueTypes <- parens $ sepBy1 valueType $ symbol "->"
     return $ ValueTypeFunction valueTypes

expression = StringLiteral <$> stringLiteral
  <|> do reserved "case"
         expr <- expression
         CaseBlock expr <$> braces caseMatches
  <|> ifExpression 
  <|> BoolExpr <$> boolExpression
  <|> ArithmeticExpr <$> arithmeticExpression
  <|> Var <$> identifier
  <|> do reserved "let"
         letBindList <- sepBy1 letVarBind comma
         LetInExpr letBindList <$> expression
  <|> do listElems <- brackets (sepBy1 listElementExpr comma)
         return $ List listElems

letVarBind =
  do ident <- identifier
     reservedOp "="
     LetVarBind ident <$> expression

listElementExpr = (
   do symbol "..."
      ListElemSpread <$> expression
  )
  <|> ListElemExpr <$> expression

ifExpression = 
  do reserved "if"
     bExpr <- boolExpression
     reserved "then"
     thenExpr <- expression
     reserved "else"
     If bExpr thenExpr <$> expression
  <|>
  do reserved "if"
     bExpr <- boolExpression
     thenExpr <- braces expression
     reserved "else"
     elseExpr <- braces expression
     return $ If bExpr thenExpr elseExpr

caseMatches = CaseMatchSeq <$> sepBy1 caseMatch endOfLine

caseMatch =
  do pttn <- parsePattern
     Case pttn <$> expression

boolExpression = buildExpressionParser boolOperators boolTerm

boolOperators = [
    [
      Prefix (reservedOp "not" >> return Not)
    ],
    [
      Infix (reservedOp "and" >> return (BBinary And)) AssocLeft,
      Infix (reservedOp "or" >> return (BBinary Or)) AssocLeft
    ]
  ]

boolTerm = boolTermParens
        <|> boolTermNoParens

boolTermParens = parens boolTermNoParens

boolTermNoParens = (reserved "true" >> return (BoolConst True))
         <|> (reserved "false" >> return (BoolConst False))
         <|> boolTermFunctionCall
         <|> BoolVar <$> identifier

boolTermFunctionCall =
  do name <- identifier
     callArgs <- callArguments
     return $ BoolFunctionCall name callArgs

arithmeticExpression = buildExpressionParser arithmeticOperators arithmeticTerm

arithmeticOperators = [
   [Prefix (reservedOp "-" >> return Neg)],
   [
      Infix (reservedOp "*" >> return (ABinary Multiply)) AssocLeft,
      Infix (reservedOp "/" >> return (ABinary Divide)) AssocLeft
   ],
   [
      Infix (reservedOp "+" >> return (ABinary Add)) AssocLeft,
      Infix (reservedOp "-" >> return (ABinary Subtract)) AssocLeft
   ]
  ]

arithmeticTerm = parens arithmeticExpression
               <|> try (FloatConst <$> float)
               <|> IntegerConst <$> integer
               <|> arithmeticTermFunctionCall
               <|> NumericVar <$> identifier

arithmeticTermFunctionCall = 
   do name <- identifier
      callArgs <- callArguments
      return $ ArithmeticFunctionCall name callArgs

callArguments = CallArgs <$> try (parens (sepBy1 expression comma))
                <|> (
                   do symbol "()"
                      return $ CallArgs []
                )

parseFile :: String -> IO Module
parseFile file =
  do program <- readFile file
     case parse guavaParser "" program of
       Left e -> print e >> fail "parse error"
       Right r -> return r

parseString :: String -> Module
parseString str =
   case parse guavaParser "" str of
      Left e -> error $ show e
      Right r -> r