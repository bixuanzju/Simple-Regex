module Regx
  (
    Pattern(..)
  , parseRegx
  ) where

import Text.Parsec
import Text.Parsec.String

data Pattern = Empty
             | Literal Char
             | Concat Pattern Pattern
             | Choose Pattern Pattern
             | Repeat Pattern
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
            s <- many (char '*')
            case length s of
              0 -> return b
              n -> return $ repeatStar n b

repeatStar :: Int -> Pattern -> Pattern
repeatStar n p
  | n == 1 = Repeat p
  | otherwise = Repeat (repeatStar (n-1) p)

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
