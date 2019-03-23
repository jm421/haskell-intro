import Data.Array
import Data.List
import Data.Ord

-- q3.1

data Edit = Insert Char 
          | Delete Char 
          | Copy Char 
          | Change Char Char
          deriving (Show, Eq)

-- q3.2

getCost :: Edit -> Int
getCost (Copy _) = 0
getCost _ = 1

cost :: [Edit] -> Int
cost [] = 0
cost (x:xs) = getCost x + cost xs 

-- q3.3

getInserts :: [Char] -> [Edit]
getInserts [] = []
getInserts (x:xs) = [Insert x]  ++ getInserts xs

transform :: [Char] -> [Char] -> [Edit]
transform [] [] = []

transform str1 str2
  | length str1 > length str2 = transform str2 str1
  | length str1 < length str2 = 
    let editPath = getInserts (drop((length str2) - 1 - (length str2 - length str1)) str2)
    in transform str1 (take (length str2 - (length str2 - length str1)) str2) ++ editPath

  | last str1 == last str2 = transform (init str1) (init str2) ++ [Copy (last str1)]
  | otherwise              = minimumBy (comparing cost) [transform (init str1) str2 ++ [Delete (last str1)],
                                                        transform str1 (init str2) ++ [Insert (last str2)],
                                                        transform (init str1) (init str2)++ [Change (last str1) (last str2)]
                                                        ]

main :: IO()
main = print(transform "Exeter" "Exmouth")
