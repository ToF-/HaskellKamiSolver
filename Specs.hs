import Test.Hspec
import Kami

main = hspec $ do
    describe "game state" $ do
        describe "is made of area that are groups of squares of same color" $ do
            it "is a success when there's only one group" $ do
                let first = [(Brown,[(0,0),(1,0),(0,1),(1,1)])
                            ,(Red,  [(2,0),(3,0),(2,1),(3,1)])]
                    second= [(Brown,[(0,0),(1,0),(0,1),(1,1)
                            ,(2,0),(3,0),(2,1),(3,1)])]
                success first `shouldBe` False
                success second `shouldBe` True
    describe "area" $ do
        it "can join another area if they are neighbors and same color" $ do
            let a = (Brown,[(0,0),(1,0)])
                b = (Red,  [(0,1),(0,2)])
                c = (Brown,[(1,1),(1,2)])
                d = (Brown,[(0,2),(2,2)])
            join a b `shouldBe` a 
            join a c `shouldBe` (Brown,[(0,0),(1,0),(1,1),(1,2)])
            join a d `shouldBe` a
                 
                
