module Books (Book(..), booklist) where

import Data.Maybe
import Data.Tree

import NmlParse

data Book = Book {
	title :: String,
	author :: String }
	deriving Show

books1 :: String
books1 = "<books>" ++
	"<book>" ++
	"<title>The Old Man and the Sea</title>" ++
	"<author>Ernest Hemingway</author>" ++
	"</book>" ++
	"<book>" ++
	"<title>The Catcher in the Rye</title>" ++
	"<author>J. D. Salinger</author>" ++
	"</book>" ++
	"</books>"

-- DECODE

booklist :: String -> Maybe [Book]
booklist s = case nml s of Just bl -> booklistNml bl; _ -> Nothing

booklistNml :: Nml -> Maybe [Book]
booklistNml (Node "books" bl) = Just $ mapMaybe book bl
booklistNml _ = Nothing

book :: Nml -> Maybe Book
book b@(Node "book" _) = case getValue "title" b of
	Just t -> case getValue "author" b of
		Just a -> Just $ Book { title = t, author = a }
		_ -> Nothing
	_ -> Nothing
book _ = Nothing

getValue :: String -> Nml -> Maybe String
getValue t b = case children t b of
	Node _ [Node v []] : _ -> Just v
	_ -> Nothing

children :: String -> Nml -> [Nml]
children tg (Node _ cs) = filter ((== tg) . rootLabel) cs

-- ENCODE

fromBooklist :: [Book] -> String
fromBooklist = undefined

fromBooklistNml :: [Book] -> Nml
fromBooklistNml = undefined

fromBook :: Book -> Nml
fromBook = undefined
