module Parser (expr, readExpr) where

import Ast
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

readExpr :: String -> Expr
readExpr str = case parse expr "fox" str of
    Left err -> error (show err)
    Right val -> val

--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
    where style = emptyDef {
              Token.reservedOpNames = ["+","-","*","/","=","<-"],
              Token.reservedNames = ["if", "then", "else"],
              Token.commentLine = "#" }

natural :: Parser Integer
natural = Token.natural lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

expr :: Parser Expr
expr = ifThenElse <|> formula

ifThenElse :: Parser Expr
ifThenElse = do
    reserved "if"
    a <- expr
    reserved "then"
    b <- expr
    reserved "else"
    c <- expr
    return $ ExprIfThenElse a b c

formula :: Parser Expr
formula = buildExpressionParser table juxta <?> "formula"
    where table = [
                [mulOp,divOp]
              , [addOp,subOp]
              ]
          addOp = Infix (reservedOp "+" >> return (ExprBinop Add)) AssocLeft
          subOp = Infix (reservedOp "-" >> return (ExprBinop Sub)) AssocLeft
          mulOp = Infix (reservedOp "*" >> return (ExprBinop Mul)) AssocLeft
          divOp = Infix (reservedOp "/" >> return (ExprBinop Div)) AssocLeft

juxta :: Parser Expr
juxta = do
    a <- atom
    args <- many arguments
    return $ foldl ExprApp a args

atom :: Parser Expr
atom = variable <|> number <|> parens expr <?> "atom"

variable :: Parser Expr
variable = ExprVar `fmap` identifier

number :: Parser Expr
number = (ExprNum . fromIntegral) `fmap` natural

arguments :: Parser [Expr]
arguments = char '(' *> sepBy expr (char ',') <* char ')'

