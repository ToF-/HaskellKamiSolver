module Kami
where
import Data.List

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
    coordx y s = [((x,y),c)  | (x,c) <- index s]

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

color = snd . head
squares = map fst

touch :: Area -> Area -> Bool
touch a b | color a /= color b = False
touch a b = any (\cd -> any (\cd' -> cd `near` cd') (squares b)) (squares a)  
    where 
    near (x,y) (x',y') = dx * dy == 0 && dx + dy == 1
        where 
        dx = abs (x - x')
        dy = abs (y - y')

join :: GameState -> GameState
join []  = []
join [a] = [a]
join (a:b:as) | touch a b = join ((a++b):join as)
              | otherwise = b:join (a:join as)



