module Kami
where
import Data.List

type GameState = [Area]
type Area = (Color,[Coords])
type Coords = (Int,Int)
data Color = Brown | Red | Blue
    deriving (Eq, Ord, Show)

success :: GameState -> Bool
success = (1==) . length

touch :: Area -> Area -> Bool
touch a b | color a /= color b = False
touch a b = any (\cd -> any (\cd' -> cd `near` cd') (squares b)) (squares a)  
    where 
    near (x,y) (x',y') = dx * dy == 0 && dx + dy == 1
        where 
        dx = abs (x - x')
        dy = abs (y - y')

color = fst
squares = snd

join :: GameState -> GameState
join []  = []
join [a] = [a]
join (a:b:as) | touch a b = join ((color a, squares a ++ squares b):join as)
              | otherwise = b:join (a:join as)

change :: Coords -> Color -> GameState -> GameState
change _ _ [] = []
change (x,y) c ((c',sq):as) | (x,y) `elem` sq = (c,sq):as
                            | otherwise       = (c',sq):change (x,y) c as

moves :: GameState -> [(Color,Coords)]
moves g = concatMap (\(c,sq) -> [(c',head sq) | c' <- colors g, c' /= c]) g
    where colors = nub . sort . map color 
