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
            

