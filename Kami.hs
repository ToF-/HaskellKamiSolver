module Kami
where
import Data.List
import Data.Ord

type GameState = [Area]
type Area = [(Coord,Color)]
type Coord = (Integer,Integer)
data Color = Brown | Blue | Cyan | Orange | Green | Red | White | Yellow 
    deriving (Eq,Ord,Show)

game :: [String] -> GameState
game = sort . map sort . join . singletons . mapGame
    where
    index :: [a] -> [(Integer,a)]
    index = zip [0..]

    coordx :: Integer -> [a] -> [(Coord,a)]
    coordx y s = [((y,x),c)  | (x,c) <- index s]

    coordy :: [[a]] -> [[(Coord,a)]]
    coordy  ss = [coordx y s | (y,s) <- index ss]
    
    singletons :: [[a]] -> [[a]]
    singletons = concatMap (map return)

    mapGame :: [[Char]] -> GameState
    mapGame = singletons . coordy . map (map charToColor)

charToColor :: Char -> Color
charToColor '#' = Brown
charToColor '.' = Blue
charToColor '*' = Orange
charToColor '@' = Red

colorToChar :: Color -> Char
colorToChar Brown = '#'
colorToChar Blue  = '.'
colorToChar Orange = '*'
colorToChar Red = '@'


color = snd . head
squares = map fst

touch :: Area -> Area -> Bool
touch a b | color a /= color b = False
touch a b = any (\cd -> any (\cd' -> cd `near` cd') (squares b)) (squares a)  
    where 
    near (y,x) (y',x') = dx * dy == 0 && dx + dy == 1
        where 
        dx = abs (x - x')
        dy = abs (y - y')

join :: GameState -> GameState
join []  = []
join [a] = [a]
join (a:b:as) | touch a b = join ((a++b):join as)
              | otherwise = b:join (a:join as)

text :: GameState -> String
text = output 
    where
    color = snd
    y = fst . fst
    same f a b = f a == f b
    toString = map (colorToChar . color)
    output = unlines . map toString . groupBy (same y) . sortBy (comparing y) . concat
