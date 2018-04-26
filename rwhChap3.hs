--importing some modules for exercises later
import Data.List

--defining a new data type

data BookInfo = Book Int String [String] deriving (Show)

--create a new value of type Book
myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

data MagazineInfo = Magazine Int String [String] deriving (Show)

data BookReview = BookReview BookInfo CustomerID String

type CustomerID = Int
type ReviewBody = String
{--we clarified the String mentioned in BookReview by giving it a 
descriptive name--}

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address 
 | CashOnDelivery
 | Invoice CustomerID
 deriving (Show)




pluralise word counts = map plural counts
 where plural 0 = "no " ++ word ++ "s"
       plural 1 = "one " ++ word
       plural n = show n ++ " " ++ word ++ "s"


data Fruit = Apple | Orange deriving (Show)
apple = "apple"
orange = "orange" 

--equational apple = Apple
--equational orange = Orange

{--whichFruit :: String -> Fruit
whichFruit f = case f of
 apple -> Apple
 orange -> Orange--}


betterFruit f = case f of
 "apple" -> Apple
 "orange" -> Orange




{--showing how to take three adjacent elements from list sliding right by
one element and after applying a function to it, return a list of 
results--} 


--EXERCISE

--1. write a function that gets the length of list
element xs = if null xs
 then 0
 else 1 + element (tail xs)

--2. add type signature to last function
element :: Num p => [a] -> p

--3. function to get mean
--mean' :: Num p => [a] -> p
mean_ xs = if null xs
 then 0
 else sum xs / fromIntegral (length xs) 

--function to turn list into palindrom
palindrom xs = if xs == reverse xs
 then xs
 else xs ++ reverse xs

--5. function to check for palindrom
checkPalin xs = xs == reverse xs 

--6. a function that sorts a list of lists based on the length of each sublist
--compSub xs = Data.List.sortBy (compare 'on' length) xs
compSub xs = sortBy (\x y -> compare (length x) (length y)) xs

--7. function that joins sublists using a separator value
charToString :: Char -> String
charToString c = [c]

joinList s xs = if null xs
 then ""
 else head xs ++ (charToString s) ++ joinList s (tail xs)

--without using the above function
joinList' s xs = if null xs
 then ""
 else head xs ++ [s] ++ joinList' s (tail xs)

--7. define the above so that separator dont appear after the last element
interS s xs = if null xs
 then ""
 else head xs ++ (if null (tail xs)
  then ""
  else [s] ++ interS s (tail xs))


--8. write above using pattern matching
interSS s [] = ""
interSS s xs = head xs ++ (if null (tail xs)
 then ""
 else [s] ++ interSS s (tail xs))

--9. tree in binarytree.hs

--10. direction data type
data Direction = Straight | LeftTurn | RightTurn deriving (Show, Eq)

{--11. Finding the direction does not require finding the angle between 
the segments. Finding z-coordinate of the cross-product of the two vectors
P1P2 and P1P3 given by expression (x2-x1)(y3-y1)-(y2-y1)(x3-x1),
if the result = 0 then collinear (straight), +ve then counter-clockwise 
(left), -ve then clockwise (right).--}
findDir [(a1,a2),(b1,b2),(c1,c2)]
 | res == 0 = Straight
 | res > 0 = LeftTurn
 | res < 0 = RightTurn
 where res = (b1 - a1)*(c2 - a2) - (b2 - a2)*(c1 - a1)

findDir' (a1,a2) (b1,b2) (c1,c2)
 | res == 0 = Straight
 | res > 0 = LeftTurn
 | res < 0 = RightTurn
 where res = (b1 - a1)*(c2 - a2) - (b2 - a2)*(c1 - a1)

{--12. function that takes a list of 2D points and returns a list of 
directions made by triples successively--}
tupS = [(1,2),(2,3),(3,4),(4,-5),(5,1)]

{--12. function that takes a list of 2D points and returns a list of 
directions made by triples successively--}
serialDir :: (Num b, Ord b) => [(b, b)] -> [Direction]
serialDir [] = []
serialDir [x] = []
serialDir [x,y] = []
serialDir [x,y,z] = [findDir x y z]
serialDir xs = [findDir (xs !! 0) (xs !! 1) (xs !! 2)]++serialDir (tail xs)