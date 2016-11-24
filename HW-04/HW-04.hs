
--Exercise 1 : Wholemeal Programming

fun1 :: [Integer] -> Integer
{-fun1 []     = 1
fun1 (x:xs)
          | even x    = (x - 2)*fun1 xs
          | otherwise = fun1 xs
-}
fun1 =product.map (\x-> x-2).filter even

fun2 :: Integer -> Integer
{-fun2 1 = 0
fun2 n 
     | even n    = n + fun2 (n ‘div‘ 2)
     | otherwise = fun2 (3*n + 1)-}
fun2 =(\x -> if x== 1 then 0 else x).sum.takeWhile(>0).(iterate (\x -> if even x then 3*x+2 else x `div` 2))

--Exercise 2 : folding with trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show,Eq)

foldTree :: [a] -> Tree a 

foldTree = foldr (\x tree -> insert x tree) Leaf

insert :: a -> Tree a -> Tree a

insert newVal Leaf                            = Node 0 Leaf newVal Leaf
insert newVal (Node h leftTree val rightTree)  
                                              |leftHeight >= rightHeight = Node (getNewHeight leftTree newRightTree) leftTree val newRightTree
                                              |otherwise                 = Node (getNewHeight rightTree newLeftTree) newLeftTree val rightTree
      where getHeight (Node h _ _ _)=h
            getHeight Leaf          =(-1)
            rightHeight             = getHeight rightTree
            leftHeight              = getHeight leftTree
            newRightTree            = insert newVal rightTree
            newLeftTree             = insert newVal leftTree
            getNewHeight t1 t2      = if getHeight t1 > getHeight t2 then (getHeight t1)+1 else (getHeight t2)+1


--Exercise 3 : More folds !

xor :: [Bool] -> Bool
xor = foldr (\x result -> if x == result then False else True)False 
       
--implement map as fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []




sieveSundaram :: Integer -> [Integer]
sieveSundaram n =map (\x -> 2*x+1) (filter (`notElem` (map (\(i,j) -> i+j+2*i*j) getIJ)) [1..limit])
    where
        getIJ = [(x,y) | x <- [1..n],y <- [x..n],x+y+2*x*y < n]
        limit = (fromIntegral n `div` 2) -1

