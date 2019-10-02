module Parser where

import Control.Monad
import Text.Parsec.Expr
import Text.Parsec
import qualified Text.Parsec.Token as Token
import Lexer

data Module = Module String Stmts
            | ModuleExposing String Exposing Stmts
            deriving (Show, Eq)

data Exposing = Exposes [String] deriving (Show, Eq)

data Stmts = Statements [Stmt] deriving (Show, Eq)

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

data DefArguments = Args [DefArgument] deriving (Show, Eq)

data DefArgument = DefArgName String | DefArgPattern Pattern deriving (Show, Eq)

data Expr = CaseBlock Expr CaseMatches
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

data CallArguments = CallArgs [Expr] deriving (Show, Eq)

data BBinOp = And | Or deriving (Show, Eq)

data CaseMatches = CaseMatchSeq [CaseMatch] deriving (Show, Eq)

data CaseMatch = Case Pattern Expr deriving (Show, Eq)

data Pattern = PatternBindingEmptyList -- []
             | PatternBindingListDestructureNElements [String] -- (x:[] x:y:[] x:y:z:[])
             | PatternBindingListDestructure [String] String -- (x:xs x:y:ys x:y:z:zs)
             deriving (Show, Eq)

guavaParser = whiteSpace >> guavaModule

guavaModule = guavaModuleWithExports 
            <|> guavaModuleNoExports

guavaModuleNoExports =
  do reserved "module"
     name <- identifier
     stmts <- statements
     return $ Module name stmts

guavaModuleWithExports =
  do reserved "module"
     name <- identifier
     reserved "exposing"
     exposes <- braces exposing
     stmts <- statements
     return $ ModuleExposing name exposes stmts

exposing =
  do list <- (sepBy1 identifier comma)
     return $ Exposes list

statements = (eof >> return (Statements []))
 <|> (
   do stmts <- (sepEndBy statement endOfLine)
      return $ Statements stmts
  )

statement = (
  do fncStmt <- functionStatement
     return $ Function fncStmt
  ) 
  <|> (
  do typeDef <- typeDefinition
     return $ TypeDefinition typeDef
  )

functionStatement = functionWithInlineTypeDef
                  <|> functionMultipleInstancesTypeDef
                  <|> functionMultipleInstancesTypeDefRef
                  <|> functionWithTypeDefRef

functionWithInlineTypeDef = 
  do reserved "function"
     name <- identifier
     defArgs <- functionDefinitionArguments
     symbol "::"
     valTypes <- valueTypes
     expr <- expression
     return $ FunctionWithInlineTypeDef name defArgs valTypes expr

functionWithTypeDefRef =
  do reserved "function"
     name <- identifier
     defArgs <- functionDefinitionArguments
     symbol "::"
     typeDefRef <- identifier
     expr <- expression
     return $ FunctionWithTypeDefRef name defArgs typeDefRef expr

functionMultipleInstancesTypeDef =
  do reserved "function"
     name <- identifier
     symbol "::"
     typeDef <- typeDefinition
     fncImpls <- functionImplementations
     return $ FunctionMultipleInstancesTypeDef name typeDef fncImpls
  
functionMultipleInstancesTypeDefRef =
  do reserved "function"
     name <- identifier
     typeDefRef <- identifier
     fncImpls <- functionImplementations
     return $ FunctionMultipleInstancesTypeDefRef name typeDefRef fncImpls

functionImplementations = (sepBy functionImplementation endOfLine)

functionImplementation =
  do name <- identifier
     defArgs <- functionDefinitionArguments
     expr <- expression
     return $ FunctionImpl name defArgs expr

functionDefinitionArguments = (
  do symbol "()"
     return $ Args []
  ) 
  <|> (parens functionDefinitionRegularArgs)

functionDefinitionRegularArgs =
  do argList <- (sepBy1 functionDefinitionArgument comma)
     return $ Args argList

functionDefinitionArgument = 
  (do argName <- identifier
      return $ DefArgName argName)
  <|> 
  (do ptn <- pattern
      return $ DefArgPattern ptn)

pattern = patternBindingEmptyList
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
     valTypes <- valueTypes
     return $ TypeDef typeName valTypes

valueTypes = sepBy1 valueType $ symbol "->"

valueType = valueTypeSingle
          <|> valueTypeList
          <|> valueTypeFunction

valueTypeSingle = 
  do typeIdent <- identifier
     return $ ValueTypeSingle typeIdent

valueTypeList =
  do typeIdent <- identifier
     symbol "[]"
     return $ ValueTypeList typeIdent

valueTypeFunction =
  do valueTypes <- parens $ sepBy1 valueType $ symbol "->"
     return $ ValueTypeFunction valueTypes

expression = (
  do reserved "case"
     expr <- expression
     matches <- (braces caseMatches)
     return $ CaseBlock expr matches
  ) 
  <|> ifExpression 
  <|> (
  do bExpr <- boolExpression
     return $ BoolExpr bExpr
  ) 
  <|> (
  do reserved "let"
     letBindList <- (sepBy1 letVarBind comma)
     expr <- expression
     return $ LetInExpr letBindList expr
  )
  <|> (
  do symbol "["
     listElems <- (sepBy1 listElementExpr comma)
     symbol "]"
     return $ List listElems
  )

letVarBind =
  do ident <- identifier
     reservedOp "="
     expr <- expression
     return $ LetVarBind ident expr

listElementExpr = (
    do symbol "..."
       expr <- expression
       return $ ListElemSpread expr
  ) 
  <|> (
    do expr <- expression
       return $ ListElemExpr expr
  )

ifExpression = ( 
  do reserved "if"
     bExpr <- boolExpression
     reserved "then"
     thenExpr <- expression
     reserved "else"
     elseExpr <- expression
     return $ If bExpr thenExpr elseExpr
  )
  <|> (
  do reserved "if"
     bExpr <- boolExpression
     thenExpr <- braces expression
     reserved "else"
     elseExpr <- braces expression
     return $ If bExpr thenExpr elseExpr
  )

caseMatches = 
  do caseMatches <- (sepBy1 caseMatch endOfLine)
     return $ CaseMatchSeq caseMatches

caseMatch =
  do pttn <- pattern
     expr <- expression
     return $ Case pttn expr

boolExpression = buildExpressionParser boolOperators boolTerm

boolOperators = [
    [
      Prefix (reservedOp "not" >> return (Not))
    ],
    [
      Infix (reservedOp "and" >> return (BBinary And)) AssocLeft,
      Infix (reservedOp "or" >> return (BBinary Or)) AssocLeft
    ]
  ]

boolTerm = boolTermParens
        <|> boolTermNoParens

boolTermParens = parens boolTermNoParens

boolTermNoParens = boolExpression
         <|> (reserved "true" >> return (BoolConst True))
         <|> (reserved "false" >> return (BoolConst False))
         <|> (
           do ident <- identifier
              return $ BoolVar ident
         )
         <|> boolTermFunctionCall

boolTermFunctionCall =
  do name <- identifier
     callArgs <- parens callArguments
     return $ BoolFunctionCall name callArgs

callArguments =
  do callArgs <- (sepBy1 expression comma)
     return $ CallArgs callArgs

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