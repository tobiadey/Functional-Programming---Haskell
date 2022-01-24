module Section8 where
import Data.Char
import Data.List


-- Trees with anything in their leaves:
-- In terminal type: Leaf 2
-- In terminal type: Branch (Leaf 2) (Leaf 4)
data LTree a = Leaf a | Branch (LTree a) (LTree a)
	deriving Show

--making constats of type Ltree to reuse them in functions below
small :: LTree Int
small = (Leaf 2)

big :: LTree Int
big = Branch (Leaf 2) (Leaf 4)

-- A general function on trees:
-- In terminal type: sumLTree small
-- In terminal type: sumLTree big
sumLTree :: Num a => LTree a -> a
sumLTree (Leaf x) = x
sumLTree (Branch l r) = sumLTree l + sumLTree r


--Another list type:
data List a = Nil | Cons a (List a)

-- Doubling each element in a tree of numbers:
-- In terminal type: doubleTree small
-- In terminal type: doubleTree big
doubleTree :: LTree Int -> LTree Int
doubleTree (Leaf x) = Leaf (2*x)
doubleTree (Branch l r) = Branch (doubleTree l) (doubleTree r)

-- Generalizing to a higher-order function on trees
-- In terminal type: mapLTree (+5) small
-- In terminal type: mapLTree (+5) big
mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (Leaf x) = Leaf (f x)
mapLTree f (Branch l r) = Branch (mapLTree f l) (mapLTree f r)


-- A tree parameterized by the data in branched (nodes):
data NTree a = Empty | Node a (NTree a) (NTree a)
	deriving Show

--making constats of type Ltree to reuse them in functions below
small2 :: NTree Int
small2 = Node 2 (Empty) (Empty)

big2 :: NTree Int
big2 = Node 2 (Node 3 (Empty) (Empty)) (Node 4 (Empty) (Empty))

-- Search trees
-- In terminal type: member 2 small2
-- In terminal type: member 2 big2
member :: Ord a => a -> NTree a -> Bool
member x Empty = False
member x (Node k l r)
	| x < k = member x l
	| x > k = member x r
	| otherwise = True

