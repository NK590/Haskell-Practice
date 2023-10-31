-- 1-1, 1-2
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- 1-3
doubleFactorial :: Int -> Int
doubleFactorial 1 = 1
doubleFactorial 2 = 2
doubleFactorial n = n * doubleFactorial (n-2)

-- 2-1
mul n 0 = 0
mul n 1 = n
mul n m = mul n (m - 1) + n

-- 2-2
power :: Int -> Int -> Int
power x 0 = 1
power x y = x * power x (y-1)

-- 2-3
plusOne x = x + 1

addition x 0 = x
addition x y = plusOne (addition x (y-1))

-- 2-4
log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 (div n 2)

-- 3-1
myReplicate :: Int -> a -> [a]
myReplicate 0 a = []
myReplicate n a = a : myReplicate (n-1) a

-- 3-2
(!!!) :: [a] -> Int -> a
[]     !!! _ = error "index error"
x      !!! 0 = head x
(x:xs) !!! n = xs !!! (n-1)

-- 3-3
myZip :: [a] -> [b] -> [(a, b)]
myZip []     _      = []
myZip _      []     = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- 3-4
myLength :: Num t => [a] -> t
myLength xs = go 0 xs
    where
        go x []     = x
        go x (_:xs) = go (x + 1) xs

main :: IO()
main = do
    print (factorial 5)
    print (factorial 1000)
    -- print(factorial (-1)) : 무한루프 발생, 인자값이 음수면 무한히 내려가면서 곱함
    print (doubleFactorial 8)
    print (doubleFactorial 7)

    print (power 3 4)
    print (addition 4 5)
    print (log2 9)

    print(myReplicate 10 'b')
    print([1, 2, 3, 4, 5] !!! 3)
    print(myZip [1, 2, 3, 4, 5] ['a', 'b', 'c', 'd'])
    print(myLength [1, 2, 3, 4, 5])