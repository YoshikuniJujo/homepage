module Analyzer (
	Analyzer, runAnalyzer, eof, spot, token,
	tokens, tokensWhile, listAll, listMap ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (liftM, ap)

import qualified ListLike as LL

data Analyzer s a = Analyzer {
	runAnalyzer :: s -> Either String (a, s) }

instance Monad (Analyzer s) where
	return = Analyzer . (Right .) . (,)
	a >>= b = Analyzer $ \s ->
		case runAnalyzer a s of
			Right (x, t) -> runAnalyzer (b x) t
			Left err -> Left err
	fail = Analyzer . const . Left

instance Functor (Analyzer s) where
	fmap = liftM

instance Applicative (Analyzer s) where
	pure = return; (<*>) = ap

eof :: LL.ListLike a => Analyzer a Bool
eof = Analyzer $ Right . ((,) <$> LL.null <*> id)

spot :: LL.ListLike a =>
	(LL.Element a -> Bool) -> Analyzer a (LL.Element a)
spot p = Analyzer $ \s -> case LL.uncons s of
	Just (h, t) | p h -> Right (h, t)
	_ -> Left "spot error"

token :: LL.ListLike a => Analyzer a (LL.Element a)
token = spot $ const True

tokens :: LL.ListLike a => Integer -> Analyzer a a
tokens = Analyzer . (Right .) . LL.splitAt

tokensWhile :: LL.ListLike a =>
	(LL.Element a -> Bool) -> Analyzer a a
tokensWhile = Analyzer . (Right .) . LL.span

listAll :: LL.ListLike a => Analyzer a b -> Analyzer a [b]
listAll = loopWhile eof

loopWhile :: Monad m => m Bool -> m a -> m [a]
loopWhile p m = do
	e <- p
	if e then return [] else
		(:) `liftM` m `ap` loopWhile p m

listMap :: LL.ListLike b =>
	(a -> Analyzer b c) -> [a] -> Analyzer b [c]
listMap = mapWhile eof

mapWhile :: Monad m => m Bool -> (a -> m b) -> [a] -> m [b]
mapWhile _ _ [] = return []
mapWhile p m (x : xs) = do
	e <- p
	if e then return [] else
		(:) `liftM` m x `ap` mapWhile p m xs
