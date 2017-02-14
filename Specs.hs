import Test.Hspec
import Data.List (sort)
import Kami

main = hspec $ do
    describe "game" $ do
        it "defines a game state by reading text" $ do
            success (game ["##"]) `shouldBe` True
            success (game ["#."]) `shouldBe` False

