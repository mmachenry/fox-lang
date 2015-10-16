module Parser (expr, readStr, definitions) where

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
    Token.commentLine = "#",
    Token.reservedOpNames = ["=","<-"] ++ concatMap (fmap fst) operators,
    Token.reservedNames = ["if", "then", "else", "match"]
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
    <*> many pattern
    <*> (reservedOp "=" *> expr)

pattern :: Parser Pattern
pattern =
        PatternId <$> identifier
    <|> parenOrTuple PatternTuple pattern

expr :: Parser Expr
expr =
        ifThenElse
    <|> match
    <|> statementBlock
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
          infl (lex, abs) = Infix (reservedOp lex >> pure (ExprBinop abs))
                                  AssocLeft

juxta :: Parser Expr
juxta = foldl1 ExprApp <$> many1 atom

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
    case items of
        [item] -> pure item
        _ -> pure $ cons items

