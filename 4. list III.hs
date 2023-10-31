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

main :: IO()
main = do
    print (myAnd [True, False, True])
    print (myOr [True, False, True])