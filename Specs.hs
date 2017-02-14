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
            (lines (text (game ["##." 
                               ,"#.."
                               ,"..@"]))) `shouldBe` ["##." 
                                                     ,"#.."
                                                     ,"..@"]
