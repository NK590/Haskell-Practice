-- 2-1
myScanr step zero [] = [zero]
myScanr step zero (x:xs) = (step x y):ys
    where ys@(y:_) = myScanr step zero xs

main :: IO()
main = do
    print (myScanr (+) 0 [1, 2, 3, 4, 5])