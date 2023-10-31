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

main :: IO()
main = do
    print (takeInt 4 [11, 21, 31, 41, 51, 61])
    print (takeInt 4 [1, 2, 3])
    print (dropInt 3 [11, 21, 31, 41, 51])
    print (dropInt 3 [1, 2])
    print (sumInt [1, 2, 3])
    print (scanSum [1, 2, 3, 4, 5])
    print (diffs [1, 2, 4, 8, 16])