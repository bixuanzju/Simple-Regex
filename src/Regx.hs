module Regx
  (
    Pattern(..)
  , parseRegx
  ) where

import Text.Parsec
import Text.Parsec.String

data Pattern = EmptyR
             | Literal Char
             | Concat Pattern Pattern
             | Choose Pattern Pattern
             | Repeat Pattern
             | OneMore Pattern
             | ZeroOne Pattern
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
              s -> return $ repeatStar s b

repeatStar :: String -> Pattern -> Pattern
repeatStar (n:ns) p = foldl f acc ns
  where f acc' n'
          | n' == '+' = OneMore acc'
          | n' == '*' = Repeat acc'
          | n' == '?' = ZeroOne acc'
        acc = case n of
                '+' -> OneMore p
                '*' -> Repeat p
                '?' -> ZeroOne p

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
