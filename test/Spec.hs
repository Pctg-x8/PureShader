import Test.Hspec
import Test.Hspec.Expectations
import PSParser

main :: IO ()
main = hspec $ do
    describe "dropComments" $
        it "drops line comments exactly" $
            parseExpression ("--line_comment\napp 3 $ 3" :@: initLocation) `shouldSatisfy` parsingSucceeded
    describe "parseNumber" $ do
        it "returns Success with LocatedString \"0123\" initLocation" $
            parseNumber ("0123" :@: initLocation) `shouldBe` Success (IntValue $ "0123" :@: initLocation, "" :@: Location 1 5)
        it "returns Failed and remains entire of input" $
            parseNumber ("abc" :@: initLocation) `shouldBe` Failed ("abc" :@: initLocation)
    describe "parseIdentifier" $ do
        it "returns Success with LocatedString \"abc\" initLocation" $
            parseIdentifier ("abc " :@: initLocation) `shouldBe` Success ("abc" :@: initLocation, " " :@: Location 1 4)
        it "returns Failed and remains entire of input" $
            parseIdentifier (" abc" :@: initLocation) `shouldBe` Failed (" abc" :@: initLocation)
        it "can parse that contains characters out of ascii" $
            parseIdentifier ("にゃーん" :@: initLocation) `shouldBe` Success ("にゃーん" :@: initLocation, "" :@: Location 1 5)
    describe "parseExpression" $ do
        it "can parse \"にゃあ\" and returns Success with IdentifierRefExpr $ LocatedString \"にゃあ\" initLocation" $
            let Success (result, _) = parseExpression ("にゃあ\n" :@: initLocation) in result `shouldBe` (IdentifierRefExpr $ "にゃあ" :@: initLocation)
        it "can parse \"(3)\" and returns same value as 3" $
            parseExpression ("( 3)" :@: initLocation) `shouldBe` Success (NumberConstExpr $ IntValue ("3" :@: Location 1 3), "" :@: Location 1 5)
        it "can parse \"vin.xyz\" and returns Success with MemberRefExpr [LocatedString \"vin\" initLocation, LocatedString \"xyz\" $ Location 1 5]" $
            let Success (result, _) = parseExpression ("vin.xyz" :@: initLocation) in
                result `shouldBe` MemberRefExpr ["vin" :@: initLocation, "xyz" :@: Location 1 5]
        it "can parse \"-3\"" $
            let Success (result, _) = parseExpression $ "-3" :@: initLocation in
                result `shouldBe` FunApplyExpr (SymbolIdentExpr $ "-" :@: initLocation) (NumberConstExpr $ IntValue ("3" :@: Location 1 2))
        it "can parse \"~ 0.0\"" $
            let Success (result, _) = parseExpression $ "~ 0.0" :@: initLocation in
                result `shouldBe` FunApplyExpr (SymbolIdentExpr $ "~" :@: initLocation) (NumberConstExpr $ FloatingValue ("0.0" :@: Location 1 3))
        it "can parse \"Vec4 0 0 0 1\"" $ case parseExpression $ "Vec4 0 0 0 1" :@: initLocation of
            Success (result, _) -> result `shouldBe` expect where
                expect = FunApplyExpr (FunApplyExpr (FunApplyExpr (FunApplyExpr name first) second) third) fourth
                name = IdentifierRefExpr $ "Vec4" :@: initLocation
                first = NumberConstExpr $ IntValue $ "0" :@: Location 1 6
                second = NumberConstExpr $ IntValue $ "0" :@: Location 1 8
                third = NumberConstExpr $ IntValue $ "0" :@: Location 1 10
                fourth = NumberConstExpr $ IntValue $ "1" :@: Location 1 12
        it "can parse \"3 3\" and returns the result which is same as \"3\"" $
            let Success (res1, _) = parseExpression $ "3 3" :@: initLocation in
                let Success (res2, _) = parseExpression $ "3" :@: initLocation in
                    res1 `shouldBe` res2
        it "can parse \"2 + 3\"" $
            let Success (res, _) = parseExpression $ "2 + 3" :@: initLocation in
            let l = NumberConstExpr $ IntValue $ "2" :@: initLocation in
            let op = SymbolIdentExpr $ "+" :@: Location 1 3 in
            let r = NumberConstExpr $ IntValue $ "3" :@: Location 1 5 in
                res `shouldBe` BinaryExpr l op r
        it "can parse \"2 `elem` arrayList\"" $
            let Success (res, _) = parseExpression $ "2 `elem` arrayList" :@: initLocation in
            let l = NumberConstExpr $ IntValue $ "2" :@: initLocation in
            let op = IdentifierRefExpr $ "elem" :@: Location 1 4 in
            let r = IdentifierRefExpr $ "arrayList" :@: Location 1 10 in
                res `shouldBe` BinaryExpr l op r
        it "can parse \"2 `elem` [2, 3, 4]\"" $
            let Success (res, _) = parseExpression $ "2 `elem` [2, 3, 4]" :@: initLocation in
            let l = NumberConstExpr $ IntValue $ "2" :@: initLocation in
            let op = IdentifierRefExpr $ "elem" :@: Location 1 4 in
            let r = ListExpr [NumberConstExpr $ IntValue $ "2" :@: Location 1 11, NumberConstExpr $ IntValue $ "3" :@: Location 1 14, NumberConstExpr $ IntValue $ "4" :@: Location 1 17] in
                res `shouldBe` BinaryExpr l op r
        it "can parse \"[1, 3 .. 10, 11]\"" $
            let Success (res, _) = parseExpression $ "[1, 3 .. 10, 11]" :@: initLocation in
            let first = NumberConstExpr $ IntValue $ "1" :@: Location 1 2 in
            let second = NumberConstExpr $ IntValue $ "3" :@: Location 1 5 in
            let third = NumberConstExpr $ IntValue $ "10" :@: Location 1 10 in
            let fourth = NumberConstExpr $ IntValue $ "11" :@: Location 1 14 in
                res `shouldBe` ListExpr [first, second, ListRange, third, fourth]
        it "can parse \"[1..]\"" $
            let Success (res, _) = parseExpression $ "[1..]" :@: initLocation in
                res `shouldBe` ListExpr [NumberConstExpr $ IntValue $ "1" :@: Location 1 2, ListRange]
        it "can parse complex expression" $
            let left = parseExpression $ "buildUVs (transformVec4 pos pmatr.persp) $ swizzleByRepeatingToVec4 uv.xz" :@: initLocation in
                left `shouldSatisfy` parsingSucceeded
    describe "parseScriptAttributes" $ do
        it "can parse \"@import Shader.Core\"" $
            let expect = Success ([ImportNode ["Shader" :@: Location 1 9, "Core" :@: Location 1 16]], "" :@: Location 1 20) in
                parseScriptAttributes ("@import Shader.Core" :@: initLocation) `shouldBe` expect
        it "can parse \"@[import Shader.Core]\"" $
            let expect = Success ([ImportNode ["Shader" :@: Location 1 10, "Core" :@: Location 1 17]], "" :@: Location 1 22) in
                parseScriptAttributes ("@[import Shader.Core]" :@: initLocation) `shouldBe` expect
        it "can parse \"@[import Shader.Core, import Quaternion]\"" $
            let expect = Success ([ImportNode ["Shader" :@: Location 1 10, "Core" :@: Location 1 17], ImportNode ["Quaternion" :@: Location 1 30]], "" :@: Location 1 41) in
                parseScriptAttributes ("@[import Shader.Core, import Quaternion]" :@: initLocation) `shouldBe` expect

parsingSucceeded :: ParseResult a -> Bool
parsingSucceeded (Success _) = True
parsingSucceeded _ = False
