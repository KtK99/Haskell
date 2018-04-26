myDrop n xs = if n <= 0 || null xs
 then xs
 else myDrop (n - 1) (tail xs)

isOdd n = mod n 2 == 1

--isOdd :: Integral a => a -> Bool
--mod :: Integral a => a -> a -> a

myTake n xs = if n <= 0 || null xs
 then []
 else head xs : myTake (n - 1) (tail xs)


--lastButOne will take the element before the last
lastButOne xs = head (tail (reverse xs))

lastButOne' xs
 | length xs <= 1 = Nothing
 | otherwise      = head (drop (length xs - 2) xs)


lastButOne_ xs = head (drop (length xs - 2) xs)
{--the function returns error message saying empty list 
when list is null or has only one element--}