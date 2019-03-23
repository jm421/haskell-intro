import Data.List

get1st (a,_,_,_) = a
get2nd (_,a,_,_) = a
get3rd (_,_,a,_) = a
get4th (_,_,_,a) = a

suko :: (Int, Int, Int, Int) -> [Int]
suko t
    = myFilter t (permutations [1..9])

myFilter :: (Int, Int, Int, Int) -> [[Int]] -> [Int]
myFilter p (x:xs)
    | (conditions p x) = x
    | otherwise = myFilter p xs

conditions :: (Int, Int, Int, Int) -> [Int] -> Bool
conditions x y
    | y!!0 + y!!1 + y!!3 + y!!4 == get1st x
    && y!!1 + y!!2 + y!!4 + y!!5 == get2nd x
    && y!!3 + y!!4 + y!!6 + y!!7 == get3rd x
    && y!!4 + y!!5 + y!!7 + y!!8 == get4th x = True
    | otherwise = False