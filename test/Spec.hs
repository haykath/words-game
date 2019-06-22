import Test.Hspec
import Lib
import Data

gwc = gridWithCoords grid

testFindWord word =
    let (Just result) = findWord gwc word
        string = map cell2char result
    in string `shouldBe` word

main :: IO ()
main = hspec $ do
    describe "formatGrid" $ 
        it "Should concatenate every line with a newline" $
            formatGrid (gridWithCoords ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"
    describe "findWord" $ do
        it "Should find words in the grid" $ do
            testFindWord "PIRU"
            testFindWord "BIGGAY"
            testFindWord "BALLS"
            testFindWord "CU"
        it "Should not find a missing word" $            
            findWord gwc "CARALHO" `shouldBe` Nothing
    describe "findWords" $ do
        it "Should return a list with the words found" $
            let found = findWords gwc hiddenWords
                asString = map (map cell2char) found
            in asString `shouldBe` hiddenWords
        it "Should not find anything and return an empty list" $
            findWords gwc ["DEUS", "JESUS", "AMOR"] `shouldBe` []
