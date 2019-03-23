import Data.List

data Morse = Dot | Dash | Gap
    deriving (Read, Show, Enum, Eq, Ord)

morseTable1 :: [Char] -> [Morse]

morseTable1 x
    | x == "A" = [Dot, Dash, Gap]
    | x == "B" = [Dash, Dot, Dot, Dot, Gap]
    | x == "C" = [Dash, Dot, Dash, Dot, Gap]
    | x == "D" = [Dash, Dot, Dot, Gap]
    | x == "E" = [Dot, Gap]
    | x == "F" = [Dot, Dot, Dash, Dot, Gap]
    | x == "G" = [Dash, Dash, Dot, Gap]
    | x == "H" = [Dot, Dot, Dot, Dot, Gap]
    | x == "I" = [Dot, Dot, Gap]
    | x == "J" = [Dot, Dash, Dash, Dash, Gap]
    | x == "K" = [Dash, Dot, Dash, Gap]
    | x == "L" = [Dot, Dash, Dot, Dot, Gap]
    | x == "M" = [Dash, Dash, Gap]
    | x == "N" = [Dash, Dot, Gap]
    | x == "O" = [Dash, Dash, Dash, Gap]
    | x == "P" = [Dot, Dash, Dash, Dot, Gap]
    | x == "Q" = [Dash, Dash, Dot, Dash, Gap]
    | x == "R" = [Dot, Dash, Dot, Gap]
    | x == "S" = [Dot, Dot, Dot, Gap]
    | x == "T" = [Dash, Gap]
    | x == "U" = [Dot, Dot, Dash, Gap]
    | x == "V" = [Dot, Dot, Dot, Dash, Gap]
    | x == "W" = [Dot, Dash, Dash, Gap]
    | x == "X" = [Dash, Dot, Dot, Dash, Gap]
    | x == "Y" = [Dash, Dot, Dash, Dash, Gap]
    | x == "Z" = [Dash, Dash, Dot, Dot, Gap]

encode :: String -> [[Morse]]

encode (y:ys)
    | ys == [] = morseTable1 [y] : []
    | otherwise = morseTable1 [y] : encode ys