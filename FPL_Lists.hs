--FPL Lists Exercise


-- The custom list type

data List t = Nil | t :. List t deriving (Eq, Ord)



-- | Returns the head of the list or the given default.
--
-- >>> headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- >>> headOr 3 Nil
-- 3
--
-- prop> \x -> x `headOr` infinity == 0
--
-- prop> \x -> x `headOr` Nil == x
--headOr :: a -> List a -> a
headOr :: a -> [a] -> a
headOr d [] = d
headOr d (x:_) = x    

-- | The product of the elements of a list.
--
-- >>> product Nil
-- 1
--
-- >>> product (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> product (1 :. 2 :. 3 :. 4 :. Nil)
-- 24
--product :: List Int -> Int
--product :: (Int a) -> [a] -> a
product' [] = 0
product' xs = foldl (*) 1 xs  


-- | Sum the elements of the list.
--
-- >>> sum (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> sum (1 :. 2 :. 3 :. 4 :. Nil)
-- 10
--
-- prop> \x -> foldLeft (-) (sum x) x == 0
--sum :: List Int -> Int
sum' [] = 0
sum' xs = foldl (+) 0 xs


-- | Return the length of the list.
--
-- >>> length (1 :. 2 :. 3 :. Nil)
-- 3
--
-- prop> \x -> sum (map (const 1) x) == length x
--length :: List a -> Int
length' [] = 0
length' xs = foldl (\acc x-> acc + 1) 0 xs


-- | Map the given function on each element of the list.
--
-- >>> map (+10) (1 :. 2 :. 3 :. Nil)
-- [11,12,13]
--
-- prop> \x -> headOr x (map (+1) infinity) == 1
--
-- prop> \x -> map id x == x
--map :: (a -> b) -> List a -> List b
map' _ [] = []
map' f xs = f (head xs) : map' f (tail xs)


-- | Return elements satisfying the given predicate.
--
-- >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- [2,4]
--
-- prop> \x -> headOr x (filter (const True) infinity) == 0
--
-- prop> \x -> filter (const True) x == x
--
-- prop> \x -> filter (const False) x == Nil
--filter :: (a -> Bool) -> List a -> List a
filter' _ [] = []
filter' p xs = foldr (\x acc -> if p x then x:acc else acc) [] xs 


-- | Append two lists to a new list.
--
-- >>> (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
-- [1,2,3,4,5,6]
--
-- prop> \x -> headOr x (Nil ++ infinity) == 0
--
-- prop> \x -> headOr x (y ++ infinity) == headOr 0 y
--
-- prop> \x -> (x ++ y) ++ z == x ++ (y ++ z)
--
-- prop> \x -> x ++ Nil == x
--(++) :: List a -> List a -> List a
fun' x y = x : y : []


--infixr 5 ++

-- | Flatten a list of lists to a list.
--
-- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
--
-- prop> \x -> headOr x (flatten (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> sum (map length x) == length (flatten x)
--flatten :: List (List a) -> List a
flatten' xs = foldr (++) []  xs

-- | Map a function then flatten to a list.
--
-- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,2,3,2,3,4,3,4,5]
--
-- prop> \x -> headOr x (flatMap id (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> flatMap id (x :: List (List Int)) == flatten x
--flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = flatten' (map f xs)

-- | Flatten a list of lists to a list (again).
-- HOWEVER, this time use the /flatMap/ function that you just wrote.
--
-- prop> \x -> let types = x :: List (List Int) in flatten x == flattenAgain x
--flattenAgain :: List (List a) -> List a
flattenAgain xs = foldr (++) [] xs 
 
{--
-- | Convert a list of optional values to an optional list of values.
--
-- * If the list contains all `Full` values, 
-- then return `Full` list of values.
--
-- * If the list contains one or more `Empty` values,
-- then return `Empty`.
--
-- * The only time `Empty` is returned is
-- when the list contains one or more `Empty` values.
--
-- >>> seqOptional (Full 1 :. Full 10 :. Nil)
-- Full [1,10]
--
-- >>> seqOptional Nil
-- Full []
--
-- >>> seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
-- Empty
--
-- >>> seqOptional (Empty :. map Full infinity)
-- Empty
seqOptional ::
  List (Optional a)
  -> Optional (List a)
seqOptional =
  error "todo: Course.List#seqOptional"

--}
-- | Find the first element in the list matching the predicate.
--
-- >>> find even (1 :. 3 :. 5 :. Nil)
-- Empty
--
-- >>> find even Nil
-- Empty
--
-- >>> find even (1 :. 2 :. 3 :. 5 :. Nil)
-- Full 2
--
-- >>> find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- Full 2
--
-- >>> find (const True) infinity
-- Full 0
--find' :: (a -> Bool) -> List a -> Optional a
find' pred xs = foldr (\x acc ->if pred x then Just x else acc) Nothing xs


-- | Determine if the length of the given list is greater than 4.
--
-- >>> lengthGT4 (1 :. 3 :. 5 :. Nil)
-- False
--
-- >>> lengthGT4 Nil
-- False
--
-- >>> lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- True
--
-- >>> lengthGT4 infinity
-- True
--lengthGT4 :: List a -> Bool 
lengthGT4 xs = if length' xs > 4 then True else False

-- | Reverse a list.
--
-- >>> reverse Nil
-- []
--
-- >>> take 1 (reverse (reverse largeList))
-- [1]
--
-- prop> \x -> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)
--
-- prop> \x -> let types = x :: Int in reverse (x :. Nil) == x :. Nil
--reverse :: List a -> List a
reverse xs = foldr (\x acc -> acc++[x]) [] xs

{--
-- | Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
--
-- >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
-- [0,1,2,3]
--
-- >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
-- [1,2,4,8]
produce ::
  (a -> a)
  -> a
  -> List a
produce f x = x :. produce f (f x)

-- | Do anything other than reverse a list.
-- Is it even possible?
--
-- >>> notReverse Nil
-- []
--
-- prop> \x -> let types = x :: List Int in notReverse x ++ notReverse y == notReverse (y ++ x)
--
-- prop> \x -> let types = x :: Int in notReverse (x :. Nil) == x :. Nil
notReverse ::
  List a
  -> List a
notReverse =
  error "todo: Is it even possible?"
--}