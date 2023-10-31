import Data.List

-- 1-1
takeInt :: Int -> [a] -> [a]
takeInt 0 _      = []
takeInt _ []     = []
takeInt n (x:xs) = x : takeInt (n-1) xs

-- 1-2
dropInt :: Int -> [a] -> [a]
dropInt 0 list   = list
dropInt _ []     = []
dropInt n (x:xs) = dropInt (n-1) xs

-- 1-3
sumInt :: Num a => [a] -> a
sumInt []     = 0
sumInt (x:xs) = x + sumInt xs

-- 1-4
scanSum :: Num a => [a] -> [a]
scanSum []       = []
scanSum [x]      = [x]
scanSum (x:y:xs) = x : scanSum ((x+y) : xs)

-- 1-5
diffs :: Num a => [a] -> [a]
diffs []       = []
diffs [x]      = []
diffs (x:y:xs) = (y-x) : diffs (y:xs)

-- 2-1
-- 1)
reverseList :: [Int] -> [Int]
reverseList []     = []
reverseList (x:xs) = reverseList xs ++ [x]
-- 2)
divisors :: Integral a => a -> [a]
divisors p = [ f | f <- [1..p], mod p f == 0]
divisorList :: [Int] -> [[Int]]
divisorList = map divisors
-- 3)
divisorReverseList :: [Int] -> [[Int]]
divisorReverseList = map reverseList . divisorList

-- 2-2
-- 1)
makePair :: [a] -> (Int, a)
makePair xs = (length xs, head xs)
myRLEEncoder :: String -> [(Int, Char)]
myRLEEncoder s = map makePair (group s)
-- 2)
expandTuple :: (Int, a) -> [a]
expandTuple (n, x) = replicate n x
myRLEDecoder :: [(Int, Char)] -> String
myRLEDecoder list = concat (map expandTuple list)

-- 3-1
-- 하스켈은 lazy evaluation을 하는 언어이기 때문에, 두 결과는 동일함

-- 3-2
dropLast :: [a] -> [a]
dropLast []     = error "Empty List"
dropLast [x]    = []
dropLast (x:xs) = x : dropLast xs

main :: IO()
main = do
    print (takeInt 4 [11, 21, 31, 41, 51, 61])
    print (takeInt 4 [1, 2, 3])
    print (dropInt 3 [11, 21, 31, 41, 51])
    print (dropInt 3 [1, 2])
    print (sumInt [1, 2, 3])
    print (scanSum [1, 2, 3, 4, 5])
    print (diffs [1, 2, 4, 8, 16])

    print (reverseList [1, 2, 3, 4, 5])
    print (divisorList [1, 2, 3, 4, 5, 6, 7, 8, 9])
    print (divisorReverseList [1, 2, 3, 4, 5, 6, 7, 8, 9])
    print (myRLEEncoder "aaaaabbaaacccc")
    print (myRLEDecoder [(3, 'a'), (4, 'b'), (2, 'c')])
    print (dropLast [1, 2, 3, 4, 5])