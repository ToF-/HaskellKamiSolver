module Kami
where
import Data.List

type GameState = [Area]
type Area = (Color,[Coord])
type Coord = (Int,Int)
data Color = Brown | Blue | Cyan | Orange | Green | Red | White | Yellow 
    deriving (Eq,Ord,Show)

game :: [String] -> GameState
game ss = map zip [0..] ss

success :: GameState -> Bool 
success _ = True

