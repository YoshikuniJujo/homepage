{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Numeric

import qualified Data.ByteString as BS

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- BS.readFile fp
	let dvd = divide 16 cnt
	putStrLn . unlines $ map (unwords8 0 . map showHex2 . BS.unpack) dvd

showHex2 :: (Integral n, Show n) => n -> String
showHex2 = padd '0' 2 . flip showHex ""

padd :: a -> Int -> [a] -> [a]
padd x0 n xs = replicate (n - length xs) x0 ++ xs

divide :: Int -> BS.ByteString -> [BS.ByteString]
divide _ "" = []
divide n bs = let (h, t) = BS.splitAt n bs in h : divide n t

unwords8 :: Int -> [String] -> String
unwords8 _ [] = ""
unwords8 _ [s] = s
unwords8 7 (s : ss) = s ++ "  " ++ unwords8 0 ss
unwords8 n (s : ss) = s ++ " " ++ unwords8 (succ n) ss
