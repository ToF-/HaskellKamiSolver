module Kami
where

type GameState = [Area]
type Area = (Color,[Coords])
type Coords = (Int,Int)
data Color = Brown | Red
    deriving (Eq, Show)

success :: GameState -> Bool
success = (1==) . length

color = fst
squares = snd

join :: Area -> Area -> Area
join a b | (color a == color b) && any (neighbor (squares b)) (squares a) = (color a,squares a ++ squares b)
         | otherwise = a
    where
    neighbor :: [Coords] -> Coords -> Bool
    neighbor cs c = any (touches c) cs
        where
        touches (x,y) (x',y') = dx*dy == 0 && dx+dy == 1
            where
            dx = abs (x-x')
            dy = abs (y-y')
