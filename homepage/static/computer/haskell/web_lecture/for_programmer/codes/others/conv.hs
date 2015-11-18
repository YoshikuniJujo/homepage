import Data.Char
import System.Random
import Numeric

main :: IO ()
main = interact $ response . head . lines

select :: [a] -> StdGen -> a
select xs = (xs !!) . fst . randomR (0, length xs - 1)

response :: String -> String
response s | Just a <- bmi s = a
response s = select responses . mkStdGen . sum $ map ord s

responses :: [String]
responses = [
	"僕はすごく単純なボットなので...",
	"そうそう",
	"同じこと言われれば同じ返事をするよ",
	"でも会話っぽくなるでしょ",
	"なかの人などいない",
	"良かったじゃん",
	"(;_;)",
	"そうなの?",
	"ちょっとよくわからない...",
	"そんなことより、昨日から何も食べてないんですけど",
	"てゆうか、ここのところまじ寝てない",
	"身長と体重教えてよ"
	]

bmi, bmi2 :: String -> Maybe String
bmi ('身' : '長' : s) = bmi2 s
bmi (c : cs) = bmi cs
bmi _ = Nothing

bmi2 (c1 : c2 : c3 : cs)
	| all isDigit [c1, c2, c3] = bmi3 (read [c1, c2, c3] :: Int) cs
bmi2 (c : cs) = bmi2 cs
bmi2 _ = Nothing

bmi3, bmi4 :: Int -> String -> Maybe String
bmi3 h ('体' : '重' : s) = bmi4 h s
bmi3 h (c : cs) = bmi3 h cs
bmi3 _ _ = Nothing

bmi4 h (c1 : c2 : cs)
	| all isDigit [c1, c2] = Just $
		"身長が" ++ show h ++ "cmで体重が" ++ show w ++ "kgだとBMIは" ++
		showFFloat (Just 1) (fromIntegral w / (fromIntegral h / 100) ^ 2 :: Double) ""
		where w = read [c1, c2] :: Int
bmi4 h (c : cs) = bmi4 h cs
bmi4 _ _ = Nothing
