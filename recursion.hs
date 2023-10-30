-- 1-1, 1-2
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- 1-3
doubleFactorial :: Int -> Int
doubleFactorial 1 = 1
doubleFactorial 2 = 2
doubleFactorial n = n * doubleFactorial (n-2)

main :: IO()
main = do
    print(factorial 5)
    print(factorial 1000)
    -- print(factorial (-1)) : 무한루프 발생, 인자값이 음수면 무한히 내려가면서 곱함
    print(doubleFactorial 8)
    print(doubleFactorial 7)