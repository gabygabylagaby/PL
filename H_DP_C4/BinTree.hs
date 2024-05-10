module BinTree where
import Data.Fixed (E0)
import UU.Parsing.Machine (AnaParser(leng))

data BinTree a = Empty
              | Node a (BinTree a) (BinTree a)
              deriving (Show, Eq, Ord)

instance Functor BinTree where
    -- fmap :: (a -> b) -> BinTree a -> BinTree b
    fmap _ Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)


instance Applicative BinTree where
    -- pure :: a -> BinTree a
    pure a = Node a Empty Empty
    -- (<*>) :: BinTree (a -> b) -> BinTree a -> BinTree b
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    (Node f leftF rightF) <*> (Node x leftX rightX) = Node (f x) (leftF <*> leftX) (rightF <*> rightX)

incrementar1 :: Num a => BinTree a -> BinTree a
incrementar1 = fmap (+1)

convertirATreeInt :: BinTree String -> BinTree Int
convertirATreeInt = fmap length

-- 
mergeTrees :: (a -> b -> c) -> BinTree a -> BinTree b -> BinTree c
mergeTrees _ Empty _ = Empty
mergeTrees _ _ Empty = Empty
mergeTrees f (Node x1 left1 right1) (Node a left2 right2) = Node (f x1 a) (mergeTrees f left1 left2) (mergeTrees f right1 right2)

-- 
mergeTrees' :: (a -> b -> c) -> BinTree a -> BinTree b -> BinTree c
mergeTrees' f binaryA binaryY = pure f <*> binaryA <*> binaryY 

result4 :: BinTree Integer
result4 = mergeTrees' (+) tree1 tree2

-- 
treeToListInorden :: BinTree a -> [a]
treeToListInorden Empty = []
treeToListInorden (Node x left right) = treeToListInorden left ++ [x] ++ treeToListInorden right

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
    | x < y     = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

listToTree :: Ord a => [a] -> BinTree a
listToTree [] = Empty
listToTree xs = Node x (listToTree left) (listToTree right)
    where
        mid = length xs `div` 2
        (left, x:right) = splitAt mid xs

fusionarArboles :: Ord a => BinTree a -> BinTree a -> BinTree a
fusionarArboles t1 t2 = listToTree $ mergeLists (treeToListInorden t1) (treeToListInorden t2)

tree1 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
tree2 = Node 6 (Node 0 Empty Empty) (Node 7 Empty Empty)

tree3 = Node "ji" (Node "ja" Empty Empty) (Node "je" Empty Empty)
tree4 = Node "ju" (Node "je" Empty Empty) (Node "ja" Empty Empty)

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes mx my = (+) <$> mx <*> my

-- addMaybes (Just 1) (Just 1)
-- Just 2