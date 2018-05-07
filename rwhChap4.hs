--RWH chap 4
import Data.Char 
import Data.List
import Data.Function 
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

--lines :: String -> [String]
--takes a sentence and splits into words and returns a list of words

--prelude has a break function
{--break :: (a->Bool)->[a]->([a], [a])
first arg is a function, which takes an element and returns a bool
second arg is the list
returns a pair, consisting of prefix (the sublist, 
before the predicate returned TRUE), a sufix which is 
the rest of the list--}

--our copy of takeWhile function
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] =  []
takeWhile' p (x:xs)
 | p x =  x : takeWhile' p xs
 | otherwise = []


--our copy of dropWhile function
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] =  []
dropWhile' p xs@(x:xs')
 | p x =  dropWhile' p xs'
 | otherwise =  xs


--span p xs is equivalent to ('takeWhile' p xs, 'dropWhile' p xs)
--our copy of span function
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' _ xs@[] =  (xs, xs)
span' p xs@(x:xs')
 | p x =  let (ys,zs) = span' p xs' in (x:ys,zs)
 | otherwise = ([],xs)


--break' p is equivalent to span' (not . p)
--our copy of break function
break' :: (a -> Bool) -> [a] -> ([a],[a])
break' _ xs@[] =  (xs, xs)
break' p xs@(x:xs')
 | p x =  ([],xs)
 | otherwise = let (ys,zs) = break p xs' in (x:ys,zs)


splitLines [] = []
splitLines cs =
 let (pre, suf) = break isLineTerminator cs
 in pre : case suf of
  ('\r':'\n':rest) -> splitLines rest
  ('\r':rest) -> splitLines rest
  ('\n':rest) -> splitLines rest
  _ -> []

isLineTerminator c = c == '\r' || c == '\n'


--Excercise 1

--Question 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)


safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)


safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)


--Question 2
{--words' breaks a string up into a list of words, which were delimited
by white space --}
words' :: String -> [String]
words' s = case dropWhile isSpace s of
 "" -> []
 s -> w : words' s'
  where (w, s') = break isSpace s


splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred xs = case dropWhile (not.pred) xs of
 [] -> []
 xs -> pre : splitWith pred suf 
  where (pre, suf) = break (not.pred) xs 


--Question 3
input = "Hey! Hey! You! You!\nI don't like your girlfriend!\nNo way! No way!\nI think you need a new one!"

firstWord inp = map (head.words) (lines inp) 
 

--Question 4
tran = "hello\nworld\n"

transpose' xs = unlines (transpose allLn)
 where allLn = lines xs


square :: [Double] -> [Double]
square [] = []
square (x:xs) = x*x : square xs


upperCase :: String -> String
upperCase [] = []
upperCase (x:xs) = toUpper x : upperCase xs


{--
base = 65521
adler32 xs = helper 1 0 xs
 where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                               b' = (a' + b) `mod` base
  in helper a' b' xs
 helper a b _ = (b `shiftL` 16) .|. a
--}


--Excercise 2

--Question 1
--write this same function described below using fold
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
 in loop acc' xs

asInt_fold :: String -> Int
asInt_fold xs = if (not.null) xs
 then let acc = 0
  in foldl (\acc x->(digitToInt x)+(10*acc)) acc xs 
 else 0


--Question 2
{--Our previously defined function can not handle a negative number
so that "-3" raises an error since "-" is not a digit--}
asInt_fold' :: String -> Int
asInt_fold' xs = if (not.null) xs
 then let acc = 0
          step acc x = (if isHexDigit x 
           then digitToInt x 
           else error "Error:Non-Digit encountered")+(10*acc) 
  in foldl step acc xs 
 else 0


--Question 3
{--the above still can not handle cases where the digit represents a
very big number, approximately more than 18 digits--}


--Question 4
--skipping this for now


--Question 5 and 6
--concat function using foldr
concat' :: [[a]] -> [a]
concat' [] = []
concat' xs = foldr (++) [] xs 


--Question 7
--definition of takeWhile using recursion
takeWhile_ _ [] = []
takeWhile_ pred (x:xs) = if pred x
 then x : takeWhile_ pred xs 
 else []

--defining using foldr
takeWhilef _ [] = []
takeWhilef pred xs = foldr (\x acc-> if pred x then x:acc else []) [] xs


--Question 8 and 9
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _  [] = []
groupBy' eq (x:xs) = (x:ys) : groupBy' eq zs
 where (ys,zs) = span (eq x) xs

--write groupBy using fold
groupByF _ [] = [] 
groupByF pred xs = (\(w,ws) -> w:ws) $ foldr stepg ([],[]) xs
 where stepg x acc = if null (fst acc) 
                      then (x : fst acc, snd acc) 
                      else if (pred x (head (fst acc))) 
                            then (x : fst acc, snd acc) 
                            else ([x], (fst acc) : (snd acc))

--Question 10
--Rewrite the following using fold : any,cycle,word,unline
--any' :: Foldable t => (a -> Bool) -> t a -> Bool
--any' p = getAny #. foldMap (Any #. p)
any' _ [] = False
any' pred xs = foldl (\acc x->if pred x then True else acc) False xs


--'cycle' creates infinite repetition of the original list. 
cycle' :: [a] -> [a]
cycle' [] = []
cycle' xs = xs' where xs' = xs ++ xs'
{--the definition below is written for only 10 repeats but it may be 
extended to inifinite repeats if [1..10] is changed to [1..]--} 
cyclef [] = []
cyclef xs = foldr (\x acc-> xs++acc) [] [1..10] 


unlines' :: [String] -> String
unlines' = concatMap (++ "\n")
-- here's a more efficient version
unlines_ [] = []
unlines_ (l:ls) = l ++ '\n' : unlines_ ls

unlinesF [] = []
unlinesF ls = foldr (\x acc-> x ++ "\n" ++ acc) [] ls


{--words using fold (code stolen from StackOverflow)
https://stackoverflow.com/questions/27571979/haskell-words-implementation
--}
step x acc = if isSpace x 
 then if null (fst acc)
  then acc
  else ([], (fst acc) : (snd acc))
 else (x : fst acc, snd acc)

wordsF xs = (\(w,ws) -> if null w then ws else w:ws) $ foldr step ([],[]) xs
