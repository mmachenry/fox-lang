module Parser (readStr, definitions, expr, pattern, statement) where

import Ast
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

readStr :: Parser a -> String -> Either ParseError a
readStr parser = parse (allOf parser) "fox"

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

binaryOperators :: [[(String, BinOp)]]
binaryOperators = [
    [("*",Mul), ("/", Div)],
    [("+", Add), ("-", Sub)],
    [(">",Gt), ("<", Lt), (">=", Gte), ("<=", Lte)],
    [("==", BoolEq), ("!=", Ne)],
    [("&&", And)],
    [("||", Or)],
    [(":=", Assign)]
    ]

unaryOperators :: [[(String, UnaryOp)]]
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
        try letBind
    <|> try effectBind
    <|> match
    <|> repeat_
    <|> run
    <|> expr

letBind :: Parser Expr
letBind = ExprLetBind <$> identifier <* reservedOp "=" <*> expr

effectBind :: Parser Expr
effectBind = ExprEffectBind <$> identifier <* reservedOp "<-" <*> expr

parameter :: Parser Parameter
parameter = Parameter
    <$> identifier
    <*> option TypeInfered (reservedOp ":" *> type_)

type_ :: Parser Type
type_ =
        try (TypeFunction <$> ((parens (commaSep type_)) <|> fmap pure type__)
                          <*> (reserved "->" *> effect)
                          <*> type_)
    <|> type__
    -- <|> reserved "bool" *> pure TypeBool

type__ :: Parser Type
type__ =
        reserved "int" *> pure TypeInt
    <|> TypeVar <$> identifier

effect :: Parser Effect
effect = option EffectInfered (
        reserved "pure" *> pure EffectPure
    <|> reserved "partial" *> pure EffectPartial
    <|> reserved "divergent" *> pure EffectDivergent
    <|> reserved "total" *> pure EffectTotal
    )

pattern :: Parser Pattern
pattern =
         try (PatternApp <$> identifier <*> parens (commaSep pattern))
     <|> PatternId <$> identifier

expr :: Parser Expr
expr =
        ifThenElse
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
matchClause = (,) <$> pattern <*> (reservedOp "->" *> statement)

repeat_ :: Parser Expr
repeat_ = ExprRepeat
    <$> (reserved "repeat" *> parens expr)
    <*> statementBlock

run :: Parser Expr
run = ExprRun <$> (reserved "run" *> braces (semiSep expr))

formula :: Parser Expr
formula = buildExpressionParser table app <?> "formula"
    -- FIXME: this assumes all infix operators happen before all prefix
    where table = fmap (fmap prefix) unaryOperators
                    ++ fmap (fmap infl) binaryOperators
          infl (lex, abs) = Infix (reservedOp lex >> pure (ExprBinOp abs))
                                  AssocLeft
          prefix (lex, abs) = Prefix (reservedOp lex *> pure (ExprUnaryOp abs))

app :: Parser Expr
app = do
    a <- atom
    args <- many arguments
    pure $ foldl ExprApp a args

arguments :: Parser [Expr]
arguments = parens (commaSep expr)

atom :: Parser Expr
atom =
        variable
    <|> number
    <|> parens expr
    <|> ExprStatementBlock <$> statementBlock
    <?> "atom"

variable :: Parser Expr
variable = ExprVar <$> identifier

number :: Parser Expr
number = (ExprNum . fromIntegral) <$> natural

