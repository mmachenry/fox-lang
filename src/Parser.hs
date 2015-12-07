module Parser (readStr, definitions, expr, pattern) where

import Ast
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Control.Applicative

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
        "true", "false",
        "pure", "partial", "total", "divergent",
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
    <*> manyExpr

manyExpr :: Parser Expr
manyExpr = letBind <|> effectBind <|> compoundExpr

letBind :: Parser Expr
letBind = ExprLetBind
    <$> try (identifier <* reservedOp "=")
    <*> (expr <* reservedOp ";")
    <*> manyExpr

effectBind :: Parser Expr
effectBind = ExprEffectBind
    <$> try (identifier <* reservedOp "<-")
    <*> (expr <* reservedOp ";")
    <*> manyExpr

compoundExpr :: Parser Expr
compoundExpr = do
    expr1 <- expr
    rest <- fmap Just (reservedOp ";" *> manyExpr) <|> pure Nothing
    case rest of
        Just otherExprs -> pure $ ExprCompound expr1 otherExprs
        Nothing -> pure expr1

parameter :: Parser Parameter
parameter = Parameter
    <$> identifier
    <*> option TypeInferred (reservedOp ":" *> type_)

type_ :: Parser Type
type_ =
        try (TypeFunction <$> (parens (commaSep type_) <|> fmap pure nonArrowType)
                          <*> (reservedOp "->" *> effect)
                          <*> type_)
    <|> nonArrowType

nonArrowType :: Parser Type
nonArrowType = typeVar <|> typeIdentifier

typeVar :: Parser Type
typeVar = TypeVar <$> (reservedOp "'" *> identifier)

typeIdentifier :: Parser Type
typeIdentifier = TypeIdentifier <$> identifier

effect :: Parser Effect
effect = option EffectInferred (
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
ifThenElse = liftA3 ExprIfThenElse
    (reserved "if" *> expr)
    (reserved "then" *> expr)
    (reserved "else" *> expr)

match :: Parser Expr
match = ExprMatch
    <$> (reserved "match" *> expr)
    <*> braces (Token.semiSep1 lexer matchClause)

matchClause :: Parser (Pattern, Expr)
matchClause = (,) <$> pattern <*> (reservedOp "->" *> expr)

repeat_ :: Parser Expr
repeat_ = ExprRepeat
    <$> (reserved "repeat" *> parens expr)
    <*> (braces manyExpr)

run :: Parser Expr
run = ExprRun <$> (reserved "run" *> braces manyExpr)

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
    <|> boolean
    <|> parens expr
    <|> braces manyExpr
    <?> "atom"

variable :: Parser Expr
variable = ExprVar <$> identifier

number :: Parser Expr
number = (ExprNum . fromIntegral) <$> natural

boolean :: Parser Expr
boolean = fmap ExprBool (
        (reserved "true" *> pure True)
    <|> (reserved "false" *> pure False))

