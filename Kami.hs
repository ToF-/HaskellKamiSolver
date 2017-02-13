module Kami
where

type GameState = [Area]
type Area = (Color,[Coords])
type Coords = (Int,Int)
data Color = Brown | Red
    deriving (Eq, Show)

success :: GameState -> Bool
success = (1==) . length
