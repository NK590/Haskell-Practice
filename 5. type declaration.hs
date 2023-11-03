-- 5-1
type Name = String

data Anniversary =
    Birthday Name Date
    | Wedding Name Name Date

data Date = Date Int Int Int

kim :: Anniversary
kim = Birthday "Kim Chul Su" (Date 1990 1 1)

parkWedding :: Anniversary
parkWedding = Wedding "Kim Chul Su" "Park Young Hee" (Date 1991 2 2)

type AnniversaryBook = [Anniversary]

anniversariesOfKim :: AnniversaryBook
anniversariesOfKim = [kim, parkWedding]

showDate :: Date -> String
showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name date) = name ++ " born " ++ showDate date
showAnniversary (Wedding name1 name2 date) = name1 ++ " married " ++ name2 ++ " on " ++ showDate date

main :: IO()
main = do
    print(showAnniversary kim)
    print(showAnniversary parkWedding)