module Language.Paradox.Eval.TypeRep.Parse (
  textToTyp
) where

import            Control.Monad                   ( void )
import            Language.Paradox.Eval.Types     ( MatchCollection (..)
                                                  , Typ (..)
                                                  )
import            Text.Megaparsec
import            Text.Megaparsec.Text

import qualified  Data.Text                       as TS
import qualified  Text.Megaparsec.Lexer           as Lexer

textToTyp
  :: TS.Text
  -> MatchCollection
textToTyp t = case runParser parseTyp'' "" t of
  Left bah -> error $ show bah
  Right v -> v

whiteSpace
  :: Parser ()
whiteSpace = Lexer.space (void spaceChar) lineComment blockComment
  where
    lineComment = Lexer.skipLineComment "--"
    blockComment = Lexer.skipBlockComment "#-" "-#"

parseTyp''
  :: Parser MatchCollection
parseTyp'' = do
  e <- parseTyp'
  whiteSpace
  eof
  return e

parseTyp
  :: Parser MatchCollection
parseTyp
    = try parseTag
  <|> try parseFun
  <|> try parseAp
  <|> try parseText

parseTyp'
  :: Parser MatchCollection
parseTyp'
  = try parseAp
 <|> try parseTag
 <|> try parseFun
 <|> try parseText

parseTypNoAp
  :: Parser MatchCollection
parseTypNoAp
    = try parseTag
  <|> try parseFun
  <|> try parseText


parseAp
  :: Parser MatchCollection
parseAp = do
  MatchCollection left <- parseTypNoAp
  void $ string " -> "
  MatchCollection right <- parseTyp
  return $ MatchCollection $ left :~> right


parseTag
  :: Parser MatchCollection
parseTag = do
  void $ char '('
  MatchCollection left <- parseTyp'
  void $ char ','
  void $ char ' '
  MatchCollection right <- parseTyp'
  void $ char ')'
  return $ MatchCollection $ TTag left right

parseFun
  :: Parser MatchCollection
parseFun = do
  void $ char '('
  MatchCollection left <- parseAp
  void $ char ')'
  return $ MatchCollection left


parseText
  :: Parser MatchCollection
parseText
    = (string "ReturnData" >> return (MatchCollection TT))
  <|> (string "String" >> return (MatchCollection TS))
  <|> ( string "Double" >> return (MatchCollection TD))
  <|> ( string "Bool" >> return (MatchCollection TB))
  <|> ( string "Integer" >> return (MatchCollection TI))
  <|> try (string "[Integer]" >> return (MatchCollection TIA))
  <|> try (string "[Double]" >> return (MatchCollection TDA))
  <|> try (string "[String]" >> return (MatchCollection TSA))
  <|> try (string "[Bool]" >> return (MatchCollection TBA))
  <|> ( string "TimeSeriesChunk" >> return (MatchCollection TTSC))
