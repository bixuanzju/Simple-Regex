-- | Parse Regular Expression

module Regx
  (
    Pattern(..)
  , parseRegx
  ) where

import Text.Parsec
import Text.Parsec.String

data Pattern = EmptyR
             | Literal Char
             | Concat Pattern Pattern -- ab
             | Choose Pattern Pattern -- a|b
             | Repeat Pattern -- a*
             | OneMore Pattern -- a+
             | ZeroOne Pattern -- a?
             deriving Show

literal :: Parser Pattern
literal = do c <- letter
             return $ Literal c

expr :: Parser Pattern
expr = term `chainl1` choiceOp

term :: Parser Pattern
term = factor `chainl1` concatOp

factor :: Parser Pattern
factor = do b <- base
            s <- many (oneOf "*+?")
            case s of
              [] -> return b
              s' -> return $ repeatPattern s' b

repeatPattern :: String -> Pattern -> Pattern
repeatPattern [] p = p
repeatPattern (n:ns) p = foldl f acc ns
  where f acc' n' = case n' of
                      '+' -> OneMore acc'
                      '*' -> Repeat acc'
                      '?' -> ZeroOne acc'
                      _ -> EmptyR
        acc = case n of
                '+' -> OneMore p
                '*' -> Repeat p
                '?' -> ZeroOne p
                _ -> EmptyR

base :: Parser Pattern
base = literal <|> (do char '('
                       e <- expr
                       char ')'
                       return e)

choiceOp :: Parser (Pattern -> Pattern -> Pattern)
choiceOp = do char '|'
              return Choose

concatOp :: Parser (Pattern -> Pattern -> Pattern)
concatOp = return Concat

parseRegx :: String -> Pattern
parseRegx s = case parse expr "regx" s of
                Right a -> a
                Left _ -> error "Parse error"
