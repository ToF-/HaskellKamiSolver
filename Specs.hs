import Test.Hspec
import Data.List (sort)
import Kami

a = (Brown,[(0,0),(1,0)])
b = (Red,  [(0,1),(0,2)])
c = (Brown,[(1,1),(1,2)])
d = (Brown,[(0,2),(1,2)])

gsSort = sort . map (\(c,sq)-> (c,sort sq))

main = hspec $ do
    describe "touch" $ do
        it "should be true if an area touches another one of same color" $ do
            (a `touch` b) `shouldBe` False
            (a `touch` c) `shouldBe` True
            (a `touch` d) `shouldBe` False

    describe "game state" $ do
        describe "is made of area that are groups of squares of same color" $ do
            it "is a success when there's only one group" $ do
                let first = [(Brown,[(0,0),(1,0),(0,1),(1,1)])
                            ,(Red,  [(2,0),(3,0),(2,1),(3,1)])]
                    second= [(Brown,[(0,0),(1,0),(0,1),(1,1)
                            ,(2,0),(3,0),(2,1),(3,1)])]
                success first `shouldBe` False
                success second `shouldBe` True
    
    describe "merge" $ do
        it "join areas that touch together and are of same color" $ do
            let a' = (Brown,[(0,0),(1,0),(1,1),(1,2),(0,2),(1,2)])
            gsSort (join [a,b,d,c]) `shouldBe` (gsSort [a',b])
    
            (gsSort (join [a,b])) `shouldBe` (gsSort [a,b])
