import Data.List

-- 1-1
-- 1)
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True
-- 2)
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 1-2
-- 1)
myMaximum :: Ord a => [a] -> a
myMaximum = foldr1 max
-- 2)
myMinimun :: Ord a => [a] -> a
myMinimun = foldr1 min

-- 1-3
myReverse :: [a] -> [a]
myReverse = foldl' myFlip []
    where
        myFlip xs x = x :xs

-- 2-1
myScanr step zero [] = [zero]
myScanr step zero (x:xs) = zero : myScanr step (step zero x) xs

-- 2-2
facList n = scanl (*) 1 [2..n]

-- 3-1
returnDivisible :: Int -> [Int] -> [Int]
returnDivisible n xs = [ x | x <- xs, mod x n == 0]

-- 3-2
choosingTails :: [[Int]] -> [[Int]]
choosingTails xs = [ tail x | x <- xs, not (null x), head x > 5 ]

-- 3-3
-- guard의 적용 순서는 중요함.

-- 3-4
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = [ x | x <- xs, f x ]
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [ f x | x <- xs ]

-- 3-5
isEven :: Int -> Bool
isEven n = (mod n 2 == 0)
doubleOfFirstForEvenSeconds :: [(Int, Int)] -> [Int]
doubleOfFirstForEvenSeconds ps = map ((2*) . fst) (filter (isEven . snd) ps)

main :: IO()
main = do
    print (myAnd [True, False, True])
    print (myOr [True, False, True])
    print (myReverse [1, 2, 3, 4, 5])

    print (myScanr (+) 0 [1, 2, 3, 4, 5])
    print (facList 5)

    print (returnDivisible 3 [1, 2, 3, 4, 5, 6, 7, 8, 9])
    print (choosingTails [[7,6,3],[],[6,4,2],[9,4,3],[5,5,5]])

    print (myFilter isEven [1, 2, 3, 4, 5])
    print (myMap (2*) [1, 2, 3 , 4, 5])

    print (doubleOfFirstForEvenSeconds [(1, 2), (2, 3), (3, 4), (4, 5)])
