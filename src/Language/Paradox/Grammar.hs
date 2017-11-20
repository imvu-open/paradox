{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Paradox.Grammar where

import            Control.Monad
import            Data.ByteString                 ( ByteString )
import            Data.List                       ( intercalate )
import            Data.Map.Strict                 ( Map )
import            Text.Megaparsec
import            Text.Megaparsec.ByteString      ( Parser )

import qualified  Data.Char                       as C
import qualified  Data.Map.Strict                 as M
import qualified  Text.Megaparsec.Expr            as Expr
import qualified  Text.Megaparsec.Lexer           as Lexer

-- Note the usage of pure STMT instead of pure $. Being extra careful to try to get use of ApplicativeDo
-- https://ghc.haskell.org/trac/ghc/ticket/11835
-- https://phabricator.haskell.org/rGHCd396996298939f647c22b547bc01f1b00e6e2fd9

data Lit
  = LitString String
  | LitInt Integer
  | LitDouble Double
  | LitBool Bool
  | LitIntArray [Integer]
  | LitDoubleArray [Double]
  | LitStringArray [String]
  | LitBoolArray [Bool]
  deriving (Show)

data Expr
  = CtrLiteral SpecdCounters
  | Literal Lit
  | Symbol String
  | Ap Expr Expr
  | InfixAp Expr Expr
  | Compose Expr Expr
  deriving (Show)

newtype ParadoxParseError
  = ParadoxParseError { ppeString :: String }
  deriving Show

parseExpr
  :: ByteString
  -> Either ParadoxParseError Expr
parseExpr stream = case parse expr "" stream of
  Left err -> Left $ ParadoxParseError $ parseErrorPretty err
  Right rs -> Right rs

expr'
  :: Parser Expr
expr'
  = Expr.makeExprParser term table <?> "expression"

expr
  :: Parser Expr
expr = do
  whiteSpace
  res <- expr'
  whiteSpace
  eof
  pure res

operatorAliases
  :: String
operatorAliases = ">"

atLeastOneSpace
  :: Parser ()
atLeastOneSpace
  = void (some (char ' ') <?> "atLeastOneSpace")

table
  :: [[Expr.Operator Parser Expr]]
table =
  [ [Expr.InfixL (try $ atLeastOneSpace *> notFollowedBy (void (oneOf ('|':'.':')':operatorAliases)) <|> eof) *> pure Ap)]
  , [Expr.InfixL (try $ between atLeastOneSpace atLeastOneSpace (char '|') *> pure InfixAp )]
  , [Expr.InfixL (try $ between atLeastOneSpace atLeastOneSpace (char '.') *> pure Compose )]
  -- The following case is almost a rewrite rule. We look for its occurrence, and let it be handled
  -- later when the appropriate expression parser comes up
  , [Expr.InfixL (try $ lookAhead $ between atLeastOneSpace atLeastOneSpace (oneOf operatorAliases) *> pure InfixAp )]
  ]

term
  :: Parser Expr
term
    = (parens expr' <?> "recursive expression")
  <|> (try (aliasParser expr') <?> "alias expression")
  <|> (try (fmap mkCtrLit countersParser) <?> "counter expression")
  <|> (try (fmap Literal litIdent) <?> "literal identifier expression")
  <|> (fmap Symbol symbolIdent <?> "symbol expression")
  <?> "simple expression"

data CtrPart
  = CtrLit String
  | CtrGlob
  | CtrQuestion
  | CtrDot
  | CtrShell String
  | CtrArray [String]
  deriving (Show, Eq)

type CtrParts
  = [CtrPart]

data SpecdCounters
  = SpecdCounters [CtrParts] (Maybe (Map String Lit))
    deriving (Show)

mkCtrLit
  :: SpecdCounters
  -> Expr
mkCtrLit = CtrLiteral

-- | This parser handles aliasing > "WORD" to | alias "WORD"
-- It must consume the ' > ' or the parser will go infinite
aliasParser
  :: Parser Expr
  -> Parser Expr
aliasParser e = string " > " *> (Ap (Symbol "alias") <$> e)

countersParser
  :: Parser SpecdCounters
countersParser = do
  cparts <- sepBy1 ctrIdent (char ',')
  params <- optional (try (whiteSpace *> parseParamSection))
  let
    res = SpecdCounters (expandParts cparts) params
  pure res

expandParts
  :: [CtrParts]
  -> [CtrParts]
expandParts = generalExpand go
  where
    go e = case e of
      CtrArray ss -> map CtrLit ss
      e' -> [e']


generalExpand
  :: forall a
   . (a -> [a])
  -> [[a]]
  -> [[a]]
generalExpand analyze parts =
  let
    eachPart = map (map reverse . flip go [[]]) parts

    go :: [a] -> [[a]] -> [[a]]
    go [] acc = acc
    go (first:rest) !acc =
      let
        res :: [a]
        res = go' first

        rep = zipWith (fmap . (:)) res $ replicateS (length res) acc

        replicateS 0 r = [r]
        replicateS n r = replicate n r

        go' = analyze
      in go rest $ concat rep
  in concat eachPart

parseParamSection
  :: Parser (M.Map String Lit)
parseParamSection = M.fromList <$> between
  (char '{' *> whiteSpace)
  (whiteSpace *> char '}')
  (sepBy parseParams (char ','))

parseParams
  :: Parser (String, Lit)
parseParams = do
  whiteSpace
  s <- lowerChar
  name <- many alphaNumChar
  whiteSpace
  void $ char '='
  whiteSpace
  val <- litIdent
  whiteSpace
  let
    res = (s : name, val)
  pure res


counterStartSymbols
  :: Parser Char
counterStartSymbols
  = oneOf "_!"

counterSymbols
  :: Parser Char
counterSymbols
  = oneOf "_-!<>+"

ctrIdent
  :: Parser CtrParts
ctrIdent = do
  n <- firstCtrPart
  void $ char '.'
  r <- sepBy1 globIdent (char '.')
  let
    res = n : CtrDot : intercalate [CtrDot] r
  pure res

globIdent
  :: Parser [CtrPart]
globIdent
    = fmap (: []) ctrShell
  <|> fmap (: []) ctrLiteralArrayParser
  <|> ctrLiteralPart

firstCtrPart
  :: Parser CtrPart
firstCtrPart = do
  s <- lowerChar <|> counterStartSymbols
  r <- many (alphaNumChar <|> counterSymbols)
  let
    res = CtrLit (s : r) 
  pure res

ctrLitParser
  :: Parser CtrPart
ctrLitParser
  = CtrLit <$> ctrLitLikeParser

ctrLitLikeParser
  :: Parser String
ctrLitLikeParser
  = fmap C.toLower <$> some (counterSymbols <|> alphaNumChar)

ctrLiteralArrayParser
  :: Parser CtrPart
ctrLiteralArrayParser = between
  (char '[')
  (char ']')
  (CtrArray <$> sepBy1 ctrLitLikeParser (char ','))

ctrLiteralPart
  :: Parser [CtrPart]
ctrLiteralPart = do
  let
    glob = fmap (const CtrGlob) (char '*')
    question = fmap (const CtrQuestion) (char '?')
  some (ctrLitParser <|> glob <|> question)


ctrShell
  :: Parser CtrPart
ctrShell
  = CtrShell <$> between
  (string "{'")
  (string "'}")
  command
  where
    command = some (alphaNumChar <|> oneOf "!@#$%^*()-_=+|[]{}:,.<>/? ")

symbolIdent
  :: Parser String
symbolIdent = do
  s <- some (alphaNumChar <|> oneOf "_-'")
  notFollowedBy (char '.')
  pure s

fullFloat
  :: Parser Double
fullFloat
  = Lexer.signed (pure ()) Lexer.float

integer
  :: Parser Integer
integer
  = Lexer.signed (pure ()) Lexer.integer


escape
  :: Parser String
escape
  = sequence [char '\\', oneOf "\\\"0nrvtbf"]

nonEscape
  :: Parser Char
nonEscape
  = noneOf "\\\"\0\n\r\v\t\b\f"

character
  :: Parser String
character
  = fmap pure nonEscape <|> escape

litIdent
  :: Parser Lit
litIdent
    = fmap LitDouble (try fullFloat)
  <|> fmap LitInt integer
  <|> fmap LitBool bool
  <|> litArray
  <|> fmap LitString quotedString

bool
  :: Parser Bool
bool = reserved' "true" True
   <|> reserved' "false" False
   <|> reserved' "True" True
   <|> reserved' "False" False
  where
    reserved' n v = reserved n *> pure v

quotedString
  :: Parser String
quotedString
  = concat <$> between
  (char '"')
  (char '"')
  (many character)

litArray
  :: Parser Lit
litArray = between (char '[') (char ']')
    $ array LitDoubleArray fullFloat
  <|> array LitIntArray integer
  <|> array LitBoolArray bool
  <|> array LitStringArray quotedString
  where
    array c a = fmap c $ try $ sepBy1 a (char ',')

integerArray
  :: Parser [Integer]
integerArray = between
  (char '[')
  (char ']')
  (sepBy1 integer (char ','))


identLetter
  :: Parser Char
identLetter
  = alphaNumChar <|> oneOf "_'"

reserved
  :: String
  -> Parser ()
reserved name = try $ do
  void $ string name
  notFollowedBy identLetter <?> ("end of " ++ show name)

parens
  :: Parser a
  -> Parser a
parens = between
  (symbol "(" *> whiteSpace)
  (whiteSpace <* symbol ")")
  where
    symbol = Lexer.symbol (pure ())

whiteSpace :: Parser ()
whiteSpace = Lexer.space (void spaceChar) lineComment blockComment
  where
    lineComment = Lexer.skipLineComment "--"
    blockComment = Lexer.skipBlockComment "#-" "-#"
