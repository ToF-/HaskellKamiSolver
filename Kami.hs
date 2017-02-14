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
game = join . singletons . mapGame

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
join = sort . map sort . join'

join' :: GameState -> GameState
join' []  = []
join' [a] = [a]
join' (a:as) = [concat js ++ a] ++ ns
    where
    (js,ns) = partition (touch a) (join' as) 

text :: GameState -> String
text = output 
    where
    color = snd
    yx = fst
    y = fst . fst
    same f a b = f a == f b
    toString = map (colorToChar . color)
    output = unlines . map toString . groupBy (same y) . sortBy (comparing yx) . concat

play :: GameState -> (Coord,Color) -> GameState
play g m = sort (map sort (join (map (changeAt m) g)))
    where
    changeAt (yx,c) a = case lookup yx a of
        Just c' -> map (\(cd,_) -> (cd,c)) a
        Nothing -> a

moves :: GameState -> [(Coord,Color)]
moves g = concatMap (\a -> [(fst (head a),c) | c <- colors, c /= snd (head a)]) g
    where
    colors = nub $ sort $ map color g
