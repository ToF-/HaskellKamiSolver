

Kami: 

There are areas of different colors in the puzzle. Each area is composed of one or several squares. 

You can change the color of an area, by selecting a color and clicking on a square of an area. Then all adjacent squares with the same initial color change their color as well.

The goal is to obtain one area, meaning all the squares of the puzzle have the same color, in a limited number of moves.

Complexity : 
If there are N distinct areas and M colors currently in the puzzle, then the number of distinct possible moves is N x (M - 1)
Each move involves *painting* the area in the new color and *extending* it to the neighbors having already that color.

- Extending an area A with other areas of same color is done by:
    - selecting among neighbors of A the areas [B,C,D] having the same color as A.
    - adding the squares of these areas to the squares of A
    - finding [E,F,G,H] the neighbors of the areas [B,C,D] that are distinct from A, as new neighbors for A
    - linking each new neighbor [E,F,G,H] to A:
        - linking E and A = adding E to neighbors of A and adding A to neighbors of E
    - removing each neighbor [B,C,D]
        - removing B from the puzzle
        - removing B = remove B from all neighbors of all areas

- Creating the puzzle from a text source is done by:
    - creating one area per character read in the source, with proper area id, color, square coordinates and neighbors
        if x > 0 then neighbors include i-1
        if x < w-1 then neighbors include i+1
        if y > 0 then neighbors include i-w
        if y < h-1 then neighbors include i+w
    - merging the initial areas by recursively painting each area in its color:
        merge p = fst $ selfPaint (p,[0]) 
            selfPaint :: (Puzzle, [AreaId]) -> (Puzzle, [AreaId])
            selfPaint (p,[]) = (p,[])
            selfPaint (p,(i:is)) = case areaWithId i p of
                Nothing -> selfPaint (p, is)
                Just a  -> selfPaint (q, js ++ is)
                where 
                q = paint p i (color a) 
                js = neighbors (areaWithId i q)
                    

        


functions:

    puzzle :: [[Color]] -> Puzzle

    
    paint :: Puzzle -> AreaId -> Color -> Puzzle
        extend :: Puzzle -> AreaId -> Puzzle
    
    areaWithId :: AreaId -> Puzzle -> Maybe Area

    neighbors :: Area -> [Area]
    squares   :: Area -> [Square]
    color     :: Area -> Color
    areaId    :: Area -> AreaId

    addNeighbor :: Area -> Area -> Area
    addSquares  :: Area -> Area -> Area
    removeNeighbor :: Area -> Area -> Area






