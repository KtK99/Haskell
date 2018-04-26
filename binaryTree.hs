--tree data type declared
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

--utility function for making a singleton tree
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

--function to insert value
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
 | x == a = Node x left right
 | x < a = Node a (treeInsert x left) right
 | x > a = Node a left (treeInsert x right)


-- finding a value in a tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
 | x == a = True
 | x < a = treeElem x left
 | x > a = treeElem x right

{--use a fold to build up a tree from a list. Start with the empty tree
and then approach a list from the right and just insert element after 
element into the accumulator tree--}
nums = [2,8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums

--height of tree
heightTree EmptyTree = 0
heightTree (Node a left right) = 1 + (if heightTree left > heightTree right
 then heightTree left
 else heightTree right)

--no of elements in tree
totTree EmptyTree = 0
totTree (Node a left right) = 1 + totTree left + totTree right