import Test.Hspec
import NumberPlace hiding (main)
import Data.Array

example1 :: Board
example1 = createBoard
    [[5,3,0 ,0,7,0 ,0,0,0]
    ,[6,0,0 ,1,9,5 ,0,0,0]
    ,[0,9,8 ,0,0,0 ,0,6,0]
    ,[8,0,0 ,0,6,0 ,0,0,3]
    ,[4,0,0 ,8,0,3 ,0,0,1]
    ,[7,0,0 ,0,2,0 ,0,0,6]
    ,[0,6,0 ,0,0,0 ,2,8,0]
    ,[0,0,0 ,4,1,9 ,0,0,5]
    ,[0,0,0 ,0,8,0 ,0,7,9]
    ]

example2 :: Board
example2 = createBoard
    [[3,5,7 ,9,6,4 ,2,8,1]
    ,[4,6,8 ,1,2,3 ,5,7,9]
    ,[9,1,2 ,5,8,7 ,4,6,3]
    ,[6,3,1 ,7,9,5 ,8,4,2]
    ,[7,2,4 ,3,1,8 ,6,9,5]
    ,[8,9,5 ,2,4,6 ,1,3,7]
    ,[1,7,6 ,4,5,9 ,3,2,8]
    ,[5,8,3 ,6,7,2 ,9,1,4]
    ,[2,4,9 ,8,3,1 ,7,5,6]
    ]


main :: IO()
main = hspec $ do
    describe "splits" $ do
        it "splits 1" $
            splits 1 [1..6] `shouldBe` [[1],[2],[3],[4],[5],[6]]
        it "splits 2" $
            splits 2 [1..6] `shouldBe` [[1,2],[3,4],[5,6]]
        it "splits 3" $
            splits 3 [1..6] `shouldBe` [[1,2,3],[4,5,6]]
    describe "splitAt3" $ do
        it "splitAt3 (1,2)" $
            splitAt3 (1,2) [1..6] `shouldBe` [[1],[2],[3,4,5,6]]
        it "splitAt3 (2,4)" $
            splitAt3 (2,4) [1..6] `shouldBe` [[1,2],[3,4],[5,6]]
    describe "horizontalElems" $ do
        it "example1" $
            head (horizontalElems example1) `shouldBe` [5,3,0 ,0,7,0 ,0,0,0]
        it "example2" $
            head (horizontalElems example2) `shouldBe` [3,5,7 ,9,6,4 ,2,8,1]
    describe "verticalElems" $ do
        it "example1" $
            head (verticalElems example1) `shouldBe` [5,6,0 ,8,4,7 ,0,0,0]
        it "example2" $
            head (verticalElems example2) `shouldBe` [3,4,9 ,6,7,8 ,1,5,2]
    describe "blockElems" $ do
        it "example1" $
            head (blockElems example1) `shouldBe` [5,3,0 ,6,0,0 ,0,9,8]
        it "example2" $
            head (blockElems example2) `shouldBe` [3,5,7 ,4,6,8 ,9,1,2]
    describe "isFinished" $ do
        it "example1" $
            isFinished example1 `shouldBe` False
        it "example2" $
            isFinished example2 `shouldBe` True
    describe "count" $ do
        it "count example1" $ do
            count example1 1 `shouldBe` 3
            count example1 2 `shouldBe` 2
            count example1 3 `shouldBe` 3
        it "count example2" $ do
            count example2 1 `shouldBe` 9
            count example2 2 `shouldBe` 9
            count example2 3 `shouldBe` 9
    describe "vacancies" $ do
        it "vacancies example1" $
            head (vacancies example1) `shouldBe` (1,3)
        it "vacancies example2" $
            vacancies example2 `shouldBe` []
    describe "anti" $ do
        it "anti tests" $ do
            anti [] `shouldBe` [1..9]
            anti [1] `shouldBe` [2..9]
            anti [8,9] `shouldBe` [1..7]
            anti [1..9] `shouldBe` []
    describe "horizontalIndices" $ do
        it "horizontalIndices tests" $ do
            horizontalIndices (1,1) `shouldBe` [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9)]
            horizontalIndices (9,9) `shouldBe` [(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9)]
    describe "berticalIndices" $ do
        it "verticalIndices tests" $ do
            verticalIndices (1,1) `shouldBe` [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1)]
            verticalIndices (9,9) `shouldBe` [(1,9),(2,9),(3,9),(4,9),(5,9),(6,9),(7,9),(8,9),(9,9)]
    describe "blockIndices" $ do
        it "blockIndices tests" $ do
            blockIndices (1,1) `shouldBe` [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
            blockIndices (5,5) `shouldBe` [(4,4),(4,5),(4,6),(5,4),(5,5),(5,6),(6,4),(6,5),(6,6)]
            blockIndices (9,9) `shouldBe` [(7,7),(7,8),(7,9),(8,7),(8,8),(8,9),(9,7),(9,8),(9,9)]
    describe "getIndices" $ do
        it "getIndices example1" $ do
            getIndices example1 1 `shouldBe` [(2,4),(5,9),(8,5)]
            getIndices example1 2 `shouldBe` [(6,5),(7,7)]
        it "getIndices example2" $ do
            getIndices example2 1 `shouldBe` [(1,9),(2,4),(3,2),(4,3),(5,5),(6,7),(7,1),(8,8),(9,6)]
    describe "removeDuplicates" $ do
        it "removeDuplicates data1" $ do
            removeDuplicates [(1,6),(1,7),(1,9),(2,7),(2,9),(6,7),(8,2)] `shouldBe` [(1,6),(6,7),(8,2)]
    describe "solveStep" $ do
        it "solveStep example1" $ do
            elems (solveStep example1) `shouldBe` [5,3,0,0,7,8,0,1,0,6,0,0,1,9,5,0,0,0,0,9,8,0,4,0,5,6,0,8,0,0,0,6,0,0,0,3,4,0,6,8,0,3,7,0,1,7,0,3,0,2,0,8,0,6,9,6,0,0,0,0,2,8,0,0,8,0,4,1,9,6,0,5,0,0,0,0,8,0,1,7,9]
        it "solveStep example2" $ do
            elems (solveStep example2) `shouldBe` [3,5,7,9,6,4,2,8,1,4,6,8,1,2,3,5,7,9,9,1,2,5,8,7,4,6,3,6,3,1,7,9,5,8,4,2,7,2,4,3,1,8,6,9,5,8,9,5,2,4,6,1,3,7,1,7,6,4,5,9,3,2,8,5,8,3,6,7,2,9,1,4,2,4,9,8,3,1,7,5,6]
    describe "solve" $ do
        it "solve example1" $ do
            elems (solve example1) `shouldBe` [5,3,4,6,7,8,9,1,2,6,7,2,1,9,5,3,4,8,1,9,8,3,4,2,5,6,7,8,5,9,7,6,1,4,2,3,4,2,6,8,5,3,7,9,1,7,1,3,9,2,4,8,5,6,9,6,1,5,3,7,2,8,4,2,8,7,4,1,9,6,3,5,3,4,5,2,8,6,1,7,9]
        it "solve example2" $ do
            elems (solve example2) `shouldBe` [3,5,7,9,6,4,2,8,1,4,6,8,1,2,3,5,7,9,9,1,2,5,8,7,4,6,3,6,3,1,7,9,5,8,4,2,7,2,4,3,1,8,6,9,5,8,9,5,2,4,6,1,3,7,1,7,6,4,5,9,3,2,8,5,8,3,6,7,2,9,1,4,2,4,9,8,3,1,7,5,6]

