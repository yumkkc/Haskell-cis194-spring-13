-- (+ 1 2)
-- (define a 1)
-- This is usually represented in plait/racket as `{define a 1} as S-Exp
-- so we need to define a S-Exp here too

import Data.Char (isDigit)

data SExp = Symbol String 
          | SExp [SExp]
          deriving Show

-- {define a 1} will be SExp [Symbol "define", Symbol "a", Symbol "1"]
-- {+ 1 2} will be SExp [Symbol "+", Symbol "1", Symbol "2"]

isDigits :: String -> Bool
isDigits [] = True
isDigits (x : xs) = isDigit x && isDigits xs

data Exp = NumE Integer
         | AddE Exp Exp 
         deriving Show

parse :: SExp -> Exp
parse (SExp (Symbol "+" : x1 : x2 : xs)) = AddE e1 e2
                             where e1 = parse x1
                                   e2 = parse x2

parse (Symbol x) = if isDigits x then NumE (read x :: Integer) else error "Expecting a number"

parse _ = error "Error syntax"


mainParse' :: [String] -> [SExp]
mainParse' [] = error "Unclosed Sexp"
mainParse' ("(" : xs) = [SExp (mainParse' xs)]
mainParse' (")" : xs) = []
mainParse' (x : xs) = Symbol x : mainParse' xs

mainParse :: String -> [SExp]
mainParse xs = mainParse' $ words xs


eval :: Exp -> Integer
eval (NumE n) = n
eval (AddE l r) = eval l + eval r

repl :: String -> Integer
repl prog = eval $ parse $ head  $ mainParse prog
