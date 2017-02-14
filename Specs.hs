import Test.Hspec
import Data.List (sort)
import Kami

main = hspec $ do
    describe "game" $ do
        it "defines a game state by reading text" $ do
            game [] `shouldBe`  []
            game ["#"] `shouldBe` [[((0,0),Brown)]]
            game ["."] `shouldBe` [[((0,0),Blue)]]
            game [".#"] `shouldBe`[[((0,0),Blue)],[((0,1),Brown)]]
            game [".#","@*"] `shouldBe`[[((0,0),Blue)],[((0,1),Brown)],[((1,0),Red)],[((1,1),Orange)]]

        it "joins group of contiguous cells of same color" $ do
            game [".."] `shouldBe` [[((0,0),Blue),((0,1),Blue)]]
            game ["##."
                 ,"#.."
                 ,"..@"] `shouldBe` [[((0,0),Brown),((0,1),Brown),((1,0),Brown)]
                                    ,[((0,2),Blue),((1,1),Blue),((1,2),Blue),((2,0),Blue),((2,1),Blue)]
                                    ,[((2,2),Red)]]
            game ["##."
                 ,"#@."
                 ,"..@"] `shouldBe` [[((0,0),Brown),((0,1),Brown),((1,0),Brown)]
                                    ,[((0,2),Blue),((1,2),Blue)]
                                    ,[((1,1),Red)]
                                    ,[((2,0),Blue),((2,1),Blue)]
                                    ,[((2,2),Red)]]
    describe "text" $ do
        it "outputs a text display of a game" $ do
            let t = ["##." 
                    ,"#.."
                    ,"..@"]
            (lines (text (game t))) `shouldBe` t
            let t = ["###.#@"
                    ,"@@###@"]
            (lines (text (game t))) `shouldBe` t

    describe "play" $ do
        it "changes the color of an area" $ do
            let g = game ["##."
                         ,"#@."
                         ,"..@"]
                g' = play g ((1,1),Blue)
            (lines (text g')) `shouldBe` ["##."  
                                         ,"#.."
                                         ,"..@"]
            length g  `shouldBe` 5
            length g' `shouldBe` 3

    describe "moves" $ do
        it "gives all possible moves for a game state" $ do
            moves (game ["#"])  `shouldBe` [] 
            moves (game ["#."]) `shouldBe` [((0,0),Blue),((0,1),Brown)]
            moves (game ["#.@"]) `shouldBe` [((0,0),Blue),((0,0),Red)
                                            ,((0,1),Brown),((0,1),Red)
                                            ,((0,2),Brown),((0,2),Blue)]

    describe "eval" $ do
        it "evaluates a move for a game given a number of moves limit" $ do
            let g = game ["#."]
            eval g 1 ((0,0),Blue) `shouldBe` Success 1 ((0,0),Blue) Nothing
            eval g 5 ((0,0),Blue) `shouldBe` Success 1 ((0,0),Blue) Nothing
            let g = game ["#.#"]
            eval g 1 ((0,0),Blue) `shouldBe` Fail
            eval g 1 ((0,1),Brown) `shouldBe` Success 1 ((0,1),Brown) Nothing
            eval g 5 ((0,0),Blue) `shouldBe` Success 2 ((0,0),Blue) (Just (Success 1 ((0,0),Brown) Nothing))
            let g = game ["#.@"]
            eval g 1 ((0,0),Blue) `shouldBe` Fail
            eval g 2 ((0,0),Blue) `shouldBe` Success 2 ((0,0),Blue) (Just (Success 1 ((0,0),Red) Nothing))
            let g = game ["#.@.#"]
            eval g 2 ((0,0),Blue) `shouldBe` Fail
            eval g 2 ((0,2),Blue) `shouldBe` Success 2 ((0,2),Blue) (Just (Success 1 ((0,1),Brown) Nothing))

    describe "result" $ do
        it "returns the list of moves to success (or fail)" $ do
            result Fail `shouldBe` [] 
            result (Success 1 ((0,1),Brown) Nothing) `shouldBe` [((0,1),Brown)]
            result (Success 2 ((0,2),Blue) (Just (Success 1 ((0,1),Brown) Nothing))) `shouldBe` 
                [((0,2),Blue),((0,1),Brown)]

    describe "solve" $ do
        it "solves the puzzle in a given number of moves" $ do
            solve (game ["#.@.#"]) 8 `shouldBe` [((0,0),Blue),((0,2),Blue),((0,0),Brown)] 
            


