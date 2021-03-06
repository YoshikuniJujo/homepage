module Parse (Token, tokens, parse) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, (***))
import Data.Char (isDigit, isAlpha, isSpace)

import Environment(Value(..), Symbol, Error(..), ErrorMessage)

data Token
	= TkSymbol Symbol
	| TkTrue | TkFalse
	| TkInteger Integer
	| TkDouble Double
	| TkChar Char
	| TkString String
	| OParen | CParen
	deriving Show

syntaxErr, readErr, tokenErr, parseErr, unkChrErr, strTrmErr :: ErrorMessage
syntaxErr = "*** SYNTAX-ERROR: "
readErr = "*** READ-ERROR: "
tokenErr = "Can't tokenize: "
parseErr = "Parse error: "
unkChrErr = "Unknown character name: #\\"
strTrmErr = "string literal not terminate"

tokens :: String -> Either Error [Token]
tokens ('(' : s) = (OParen :) <$> tokens s
tokens (')' : s) = (CParen :) <$> tokens s
tokens ('#' : 'f' : s) = (TkFalse :) <$> tokens s
tokens ('#' : 't' : s) = (TkTrue :) <$> tokens s
tokens ('#' : '\\' : s) = tkChar s
tokens ('"' : s) = uncurry ($) . ((<$>) . (:) . TkString *** tokens) =<< tkString s
tokens str@(c : s)
	| isSymbolChar c, let (sm, s') = (c :) `first` span isSymbolChar s =
		(TkSymbol sm :) <$> tokens s'
	| isDigit c, let (ds, s') = (c :) `first` span isNumChar s =
		(<$> tokens s') . (:) $ if '.' `elem` ds
			then TkDouble . read $ ds ++ "0"
			else TkInteger $ read ds
	| isSpace c = tokens s
	| otherwise = Left . Error $ syntaxErr ++ tokenErr ++ show str
tokens _ = return []

tkChar :: String -> Either Error [Token]
tkChar s = case span (not . ((||) <$> isSpace <*> (`elem` "()"))) s of
	("space", s') -> (TkChar ' ' :) <$> tokens s'
	("tab", s') -> (TkChar '\t' :) <$> tokens s'
	("newline", s') -> (TkChar '\n' :) <$> tokens s'
	("return", s') -> (TkChar '\r' :) <$> tokens s'
	([c], s') -> (TkChar c :) <$> tokens s'
	(cn, _) -> Left . Error $ readErr ++ unkChrErr ++ cn

tkString :: String -> Either Error (String, String)
tkString ('"' : s) = Right ("", s)
tkString ('\\' : e : s) = (<$> tkString s) . first . (:)
	$ case e of 't' -> '\t'; 'n' -> '\n'; 'r' -> '\r'; _ -> e
tkString (c : s) = first (c :) <$> tkString s
tkString _ = Left . Error $ readErr ++ strTrmErr

isSymbolChar :: Char -> Bool
isSymbolChar c = any ($ c) [isAlpha, (`elem` "+-*/<=>?")]

isNumChar :: Char -> Bool
isNumChar c = any ($ c) [isDigit, (`elem` ".")]

parse :: [Token] -> Either Error [Value]
parse [] = return []
parse ts = do
	(v, ts') <- parse1 ts
	(v :) <$> parse ts'

parse1, parseList :: [Token] -> Either Error (Value, [Token])
parse1 (TkSymbol s : ts) = return (Symbol s, ts)
parse1 (TkFalse : ts) = return (Bool False, ts)
parse1 (TkTrue : ts) = return (Bool True, ts)
parse1 (TkInteger i : ts) = return (Integer i, ts)
parse1 (TkDouble d : ts) = return (Double d, ts)
parse1 (TkChar c : ts) = return (Char c, ts)
parse1 (TkString s : ts) = return (String s, ts)
parse1 (OParen : ts) = parseList ts
parse1 ts = Left . Error $ syntaxErr ++ parseErr ++ show ts

parseList (CParen : ts) = return (Nil, ts)
parseList ts = do
	(v, ts') <- parse1 ts
	(vs, ts'') <- parseList ts'
	return $ (v `Cons` vs, ts'')
