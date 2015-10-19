module Parser (readStr, definitions, expr, pattern, statement) where

import Ast
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

readStr :: Parser a -> String -> Either ParseError a
readStr parser str = parse (allOf parser) "fox" str

allOf :: Parser a -> Parser a
allOf p = Token.whiteSpace lexer *> p <* eof

--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser $ emptyDef {
    --Token.commentLine = "#",
    Token.reservedOpNames =
        ["=","<-","->",":"]
        ++ concatMap (fmap fst) binaryOperators
        ++ concatMap (fmap fst) unaryOperators,
    Token.reservedNames = [
        "if", "then", "else",
        "match", "repeat",
        "run"]
    }

binaryOperators = [
    [("*",Mul), ("/", Div)],
    [("+", Add), ("-", Sub)],
    [(">",Gt), ("<", Lt), (">=", Gte), ("<=", Lte)],
    [("==", BoolEq), ("!=", Ne)],
    [("&&", And)],
    [("||", Or)],
    [(":=", Assign)]
    ]

unaryOperators = [
    [("!", Dereference)]
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

definitions :: Parser Module
definitions = Module <$> many definition

definition :: Parser Definition
definition = Definition
    <$> identifier
    <*> parens (commaSep parameter)
    <*> statementBlock

statementBlock :: Parser [Expr]
statementBlock = braces (semiSep statement)

statement :: Parser Expr
statement =
        try (ExprLetBind <$> identifier <* reservedOp "=" <*> expr)
    <|> try (ExprEffectBind <$> identifier <* reservedOp "<-" <*> expr)
    <|> expr

parameter :: Parser Parameter
parameter = Parameter
    <$> identifier
    <*> option TypeInfered (reservedOp ":" *> type_)

type_ :: Parser Type
type_ = reserved "int" *> pure TypeInt

pattern :: Parser Pattern
pattern =
        PatternId <$> identifier
    <|> parenOrTuple PatternTuple pattern

expr :: Parser Expr
expr =
        ifThenElse
    <|> match
    <|> repeat_
    <|> run
    <|> formula

ifThenElse :: Parser Expr
ifThenElse = ExprIfThenElse
    <$> (reserved "if" *> expr)
    <*> (reserved "then" *> expr)
    <*> (reserved "else" *> expr)

match :: Parser Expr
match = ExprMatch
    <$> (reserved "match" *> expr)
    <*> braces (Token.semiSep1 lexer matchClause)

matchClause :: Parser (Pattern, Expr)
matchClause = (,) <$> pattern <*> (reservedOp "->" *> expr)

repeat_ :: Parser Expr
repeat_ = ExprRepeat
    <$> (reserved "repeat" *> parens expr)
    <*> statementBlock

run :: Parser Expr
run = ExprRun <$> braces (semiSep expr)

formula :: Parser Expr
formula = buildExpressionParser table app <?> "formula"
    where table = fmap (fmap prefix) unaryOperators
                    ++ fmap (fmap infl) binaryOperators
          infl (lex, abs) = Infix (reservedOp lex >> pure (ExprBinOp abs))
                                  AssocLeft
          prefix (lex, abs) = Prefix (reservedOp lex *> pure (ExprUnaryOp abs))

app :: Parser Expr
app = do
    a <- atom
    args <- many arguments
    return $ foldl ExprApp a args

arguments :: Parser [Expr]
arguments = parens (commaSep expr)

atom :: Parser Expr
atom =
        variable
    <|> number
    <|> parenOrTuple ExprTuple expr
    <?> "atom"

variable :: Parser Expr
variable = ExprVar <$> identifier

number :: Parser Expr
number = (ExprNum . fromIntegral) <$> natural

parenOrTuple :: ([a] -> a) -> Parser a -> Parser a
parenOrTuple cons parser = do
    items <- parens (Token.commaSep1 lexer parser)
    pure $ case items of
        [item] -> item
        _ -> cons items

