module Parser (expr, readModule, readExpr) where

import Ast
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

allOf :: Parser a -> Parser a
allOf p = do
    Token.whiteSpace lexer
    r <- p
    eof
    return r

readModule :: String -> Module
readModule str = case parse (allOf definitions) "fox" str of
    Left err -> error (show err)
    Right val -> val

readExpr :: String -> Expr
readExpr str = case parse (allOf expr) "fox" str of
    Left err -> error (show err)
    Right val -> val

--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser $ emptyDef {
    Token.commentLine = "#",
    Token.reservedOpNames = ["=","<-"] ++ concatMap (fmap fst) operators,
    Token.reservedNames = ["if", "then", "else"]
    }

operators = [
    [("*",Mul), ("/", Div)],
    [("+", Add), ("-", Sub)],
    [(">",Gt), ("<", Lt), (">=", Gte), ("<=", Lte)],
    [("==", BoolEq), ("!=", Ne)],
    [("&&", And)],
    [("||", Or)]
    ]

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

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

data Statement = SLet String Expr | SEffect String Expr | SExpr Expr

definitions :: Parser Module
definitions = Module <$> many definition

definition :: Parser Definition
definition = Definition
    <$> identifier
    <*> parameters
    <*> (reservedOp "=" *> expr)

parameters :: Parser [Pattern]
parameters = many pattern

pattern :: Parser Pattern
pattern =
    PatternId <$> identifier
    <|> PatternTuple <$> parens (commaSep pattern)

expr :: Parser Expr
expr =
        ifThenElse
    <|> statementBlock
    <|> formula

ifThenElse :: Parser Expr
ifThenElse = ExprIfThenElse
    <$> (reserved "if" *> expr)
    <*> (reserved "then" *> expr)
    <*> (reserved "else" *> expr)

statementBlock :: Parser Expr
statementBlock = foldStatement <$> braces (semiSep statement)
    where foldStatement :: [Statement] -> Expr
          foldStatement [] = error "empty statement block"
          foldStatement ((SLet i v):es) = ExprLetBind i v (foldStatement es)
          foldStatement ((SEffect i v):es) =
              ExprEffectBind i v (foldStatement es)
          foldStatement [(SExpr e)] = e
          foldStatement ((SExpr e):es) = ExprCompound e (foldStatement es)

statement :: Parser Statement
statement = do
        try (SLet <$> identifier <* reservedOp "=" <*> expr)
    <|> try (SEffect <$> identifier <* reservedOp "<-" <*> expr)
    <|> (SExpr <$> expr)

formula :: Parser Expr
formula = buildExpressionParser table juxta <?> "formula"
    where table = fmap (fmap infl) operators
          infl (lex, abs) = Infix (reservedOp lex >> return (ExprBinop abs))
                                  AssocLeft

juxta :: Parser Expr
juxta = do
    a <- atom
    args <- many arguments
    return $ foldl ExprApp a args

atom :: Parser Expr
atom = variable <|> number <|> parens expr <?> "atom"

variable :: Parser Expr
variable = ExprVar <$> identifier

number :: Parser Expr
number = (ExprNum . fromIntegral) <$> natural

arguments :: Parser [Expr]
arguments = parens $ commaSep expr

