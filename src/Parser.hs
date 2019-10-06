module Parser where

import Control.Monad
import Text.Parsec.Expr
import Text.Parsec
import qualified Text.Parsec.Token as Token
import Lexer

data Module = Module String Exposing Stmts deriving (Show, Eq)

newtype Exposing = Exposes [String] deriving (Show, Eq)

newtype Stmts = Statements [Stmt] deriving (Show, Eq)

data Stmt = Function FunctionStmt
          | TypeDefinition TypeDef
          deriving (Show, Eq)

data FunctionStmt = FunctionWithInlineTypeDef String DefArguments [ValueType] Expr
                  | FunctionMultipleInstancesTypeDef String TypeDef [FunctionImpl]
                  | FunctionMultipleInstancesTypeDefRef String String [FunctionImpl]
                  | FunctionWithTypeDefRef String DefArguments String Expr
                  deriving (Show, Eq)

data FunctionImpl = FunctionImpl String DefArguments Expr deriving (Show, Eq)

data TypeDef = TypeDef String [ValueType] deriving (Show, Eq)

data ValueType = ValueTypeSingle String |
                 ValueTypeList String |
                 ValueTypeFunction [ValueType] deriving (Show, Eq)

newtype DefArguments = Args [DefArgument] deriving (Show, Eq)

data DefArgument = DefArgName String | DefArgPattern Pattern deriving (Show, Eq)

data Expr = StringLiteral String
          | IntegerConst Integer
          | FloatConst Double
          | CaseBlock Expr CaseMatches
          | If BExpr Expr Expr
          | BoolExpr BExpr
          | LetInExpr [LetVarBind] Expr
          | List [ListElementExpr]
          deriving (Show, Eq)

data ListElementExpr = ListElemSpread Expr 
                     | ListElemExpr Expr
                     deriving (Show, Eq)

data LetVarBind = LetVarBind String Expr deriving (Show, Eq)

data BExpr = BoolConst Bool
           | BoolVar String
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | BoolFunctionCall String CallArguments
           deriving (Show, Eq)

newtype CallArguments = CallArgs [Expr] deriving (Show, Eq)

data BBinOp = And | Or deriving (Show, Eq)

newtype CaseMatches = CaseMatchSeq [CaseMatch] deriving (Show, Eq)

data CaseMatch = Case Pattern Expr deriving (Show, Eq)

data Pattern = PatternBindingEmptyList -- []
             | PatternBindingListDestructureNElements [String] -- (x:[] x:y:[] x:y:z:[])
             | PatternBindingListDestructure [String] String -- (x:xs x:y:ys x:y:z:zs)
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

functionDefinitionArguments =
  do symbol "()"
     return $ Args []
  <|> parens functionDefinitionRegularArgs

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
  <|> try (FloatConst <$> float)
  <|> IntegerConst <$> integer
  <|> do reserved "case"
         expr <- expression
         CaseBlock expr <$> braces caseMatches
  <|> ifExpression 
  <|> BoolExpr <$> boolExpression
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
         <|> BoolVar <$> identifier
         <|> boolTermFunctionCall

boolTermFunctionCall =
  do name <- identifier
     callArgs <- parens callArguments
     return $ BoolFunctionCall name callArgs

callArguments = CallArgs <$> sepBy1 expression comma

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