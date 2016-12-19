module Lib
  (
  readExpr
  )
where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data SemeVal = Atom String
             | String String
             | Number Integer
             | Bool Bool
             | Call SemeVal [SemeVal]
             deriving (Show)

data SemeExp = Assign SemeVal SemeVal
             | Block [SemeExp]
             | Func SemeVal [SemeVal] SemeExp
             deriving (Show)

symbol :: Parser Char
symbol = oneOf "#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parseAtom :: Parser SemeVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t"  -> Bool True
    "#f"  -> Bool False
    _     -> Atom atom

parseString :: Parser SemeVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseNumber :: Parser SemeVal
parseNumber = liftM (Number . read) $ many1 digit

parseCallWithArgs :: Parser SemeVal
parseCallWithArgs = do
  whitespace
  identifier <- parseAtom
  char '('
  whitespace
  args <- sepBy parseExp spaces
  whitespace
  char ')'
  whitespace
  return $ Call identifier args

parseCallWithoutArgs :: Parser SemeVal
parseCallWithoutArgs = do
  whitespace
  identifier <- parseAtom
  char '!'
  whitespace
  return $ Call identifier []

parseFuncCall :: Parser SemeVal
parseFuncCall = try parseCallWithoutArgs <|> parseCallWithArgs

parseExp :: Parser SemeVal
parseExp = try ( parseFuncCall
        <|> parseAtom
        <|> parseString
        <|> parseNumber)

parseAssign :: Parser SemeExp
parseAssign = do
  identifier <- parseAtom
  whitespace
  char '='
  whitespace
  val <- parseExp
  return $ Assign identifier val

parseBlock :: Parser SemeExp
parseBlock = do
  whitespace
  char '{'
  whitespace
  vals <- many (parseAssign)
  whitespace
  char '}'
  whitespace
  return $ Block vals

parseFunc :: Parser SemeExp
parseFunc = do
  whitespace
  identifier <- parseAtom
  whitespace
  cont <- parseBlock
  whitespace
  return $ Func identifier [] cont

program :: Parser [SemeExp]
program = many parseFunc

showArgs :: [SemeVal] -> String
showArgs [] = ""
showArgs [x] = showVal x
showArgs (x:xs) = (showVal x) ++ "," ++ (showArgs xs)

showVal :: SemeVal -> String
showVal (Atom a) = a
showVal (String s) = "\""++s++"\""
showVal (Number n) = show n
showVal (Bool True) = "true"
showVal (Bool False) = "false"
showVal (Call name args) = (showVal name) ++ "(" ++ (showArgs args) ++ ")"

showExp :: SemeExp -> String
showExp (Block expr) = concatMap recShow expr where
  recShow expression = case expression of
     (Assign _ _)-> showExp expression
showExp (Func name args block) = "function " ++ (showVal name) ++ "()\n" ++ (showExp block) ++ "\nend\n"
showExp (Assign name val) = (showVal name) ++ " = " ++ (showVal val)

readExpr :: String -> String
readExpr input = case parse program "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> concatMap showExp val
