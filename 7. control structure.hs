fakeIf :: Bool -> a -> a -> a
fakeIf condition ifTrue ifFalse = 
    case condition of
        True  -> ifTrue
        False -> ifFalse

main :: IO()
main = do
    print (fakeIf (1 > 0) 1 0)