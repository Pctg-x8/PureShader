import Test.Hspec
import Test.Hspec.Expectations
import PSParser

main :: IO ()
main = hspec $ do
    describe "dropComments" $
        it "drops line comments exactly" $
            parseExpression (LocatedString "--line_comment\napp 3 $ 3" initLocation) `shouldSatisfy` parsingSucceeded
    describe "parseNumber" $ do
        it "returns Success with LocatedString \"0123\" initLocation" $
            parseNumber (LocatedString "0123" initLocation) `shouldBe` Success (IntValue $ LocatedString "0123" initLocation, LocatedString "" $ Location 1 5)
        it "returns Failed and remains entire of input" $
            parseNumber (LocatedString "abc" initLocation) `shouldBe` Failed (LocatedString "abc" initLocation)
    describe "parseIdentifier" $ do
        it "returns Success with LocatedString \"abc\" initLocation" $
            parseIdentifier (LocatedString "abc " initLocation) `shouldBe` Success (LocatedString "abc" initLocation, LocatedString " " $ Location 1 4)
        it "returns Failed and remains entire of input" $
            parseIdentifier (LocatedString " abc" initLocation) `shouldBe` Failed (LocatedString " abc" initLocation)
        it "can parse that contains characters out of ascii" $
            parseIdentifier (LocatedString "にゃーん" initLocation) `shouldBe` Success (LocatedString "にゃーん" initLocation, LocatedString "" $ Location 1 5)
    describe "parseExpression" $ do
        it "can parse \"にゃあ\" and returns Success with IdentifierRefExpr $ LocatedString \"にゃあ\" initLocation" $
            let Success (result, _) = parseExpression (LocatedString "にゃあ\n" initLocation) in result `shouldBe` (IdentifierRefExpr $ LocatedString "にゃあ" initLocation)
        it "can parse \"(3)\" and returns same value as 3" $
            parseExpression (LocatedString "( 3)" initLocation) `shouldBe` Success (NumberConstExpr $ IntValue (LocatedString "3" $ Location 1 3), LocatedString "" $ Location 1 5)
        it "can parse \"vin.xyz\" and returns Success with MemberRefExpr [LocatedString \"vin\" initLocation, LocatedString \"xyz\" $ Location 1 5]" $
            let Success (result, _) = parseExpression (LocatedString "vin.xyz" initLocation) in
                result `shouldBe` MemberRefExpr [LocatedString "vin" initLocation, LocatedString "xyz" $ Location 1 5]
        it "can parse \"-3\"" $
            let Success (result, _) = parseExpression $ LocatedString "-3" initLocation in
                result `shouldBe` FunApplyExpr (SymbolIdentExpr $ LocatedString "-" initLocation) (NumberConstExpr $ IntValue (LocatedString "3" $ Location 1 2))
        it "can parse \"~ 0.0\"" $
            let Success (result, _) = parseExpression $ LocatedString "~ 0.0" initLocation in
                result `shouldBe` FunApplyExpr (SymbolIdentExpr $ LocatedString "~" initLocation) (NumberConstExpr $ FloatingValue (LocatedString "0.0" $ Location 1 3))
        it "can parse \"Vec4 0 0 0 1\"" $ case parseExpression $ LocatedString "Vec4 0 0 0 1" initLocation of
            Success (result, _) -> result `shouldBe` expect where
                expect = FunApplyExpr (FunApplyExpr (FunApplyExpr (FunApplyExpr name first) second) third) fourth
                name = IdentifierRefExpr $ LocatedString "Vec4" initLocation
                first = NumberConstExpr $ IntValue $ LocatedString "0" (Location 1 6)
                second = NumberConstExpr $ IntValue $ LocatedString "0" (Location 1 8)
                third = NumberConstExpr $ IntValue $ LocatedString "0" (Location 1 10)
                fourth = NumberConstExpr $ IntValue $ LocatedString "1" (Location 1 12)
        it "can parse \"3 3\" and returns the result which is same as \"3\"" $
            let Success (res1, _) = parseExpression $ LocatedString "3 3" initLocation in
                let Success (res2, _) = parseExpression $ LocatedString "3" initLocation in
                    res1 `shouldBe` res2
        it "can parse \"2 + 3\"" $
            let Success (res, _) = parseExpression $ LocatedString "2 + 3" initLocation in
            let l = NumberConstExpr $ IntValue $ LocatedString "2" initLocation in
            let op = SymbolIdentExpr $ LocatedString "+" $ Location 1 3 in
            let r = NumberConstExpr $ IntValue $ LocatedString "3" $ Location 1 5 in
                res `shouldBe` BinaryExpr l op r
        it "can parse \"2 `elem` arrayList\"" $
            let Success (res, _) = parseExpression $ LocatedString "2 `elem` arrayList" initLocation in
            let l = NumberConstExpr $ IntValue $ LocatedString "2" initLocation in
            let op = IdentifierRefExpr $ LocatedString "elem" $ Location 1 4 in
            let r = IdentifierRefExpr $ LocatedString "arrayList" $ Location 1 10 in
                res `shouldBe` BinaryExpr l op r
        it "can parse \"2 `elem` [2, 3, 4]\"" $
            let Success (res, _) = parseExpression $ LocatedString "2 `elem` [2, 3, 4]" initLocation in
            let l = NumberConstExpr $ IntValue $ LocatedString "2" initLocation in
            let op = IdentifierRefExpr $ LocatedString "elem" $ Location 1 4 in
            let r = ListExpr [NumberConstExpr $ IntValue $ LocatedString "2" $ Location 1 11, NumberConstExpr $ IntValue $ LocatedString "3" $ Location 1 14, NumberConstExpr $ IntValue $ LocatedString "4" $ Location 1 17] in
                res `shouldBe` BinaryExpr l op r
        it "can parse \"[1, 3 .. 10, 11]\"" $
            let Success (res, _) = parseExpression $ LocatedString "[1, 3 .. 10, 11]" initLocation in
            let first = NumberConstExpr $ IntValue $ LocatedString "1" $ Location 1 2 in
            let second = NumberConstExpr $ IntValue $ LocatedString "3" $ Location 1 5 in
            let third = NumberConstExpr $ IntValue $ LocatedString "10" $ Location 1 10 in
            let fourth = NumberConstExpr $ IntValue $ LocatedString "11" $ Location 1 14 in
                res `shouldBe` ListExpr [first, second, ListRange, third, fourth]
        it "can parse \"[1..]\"" $
            let Success (res, _) = parseExpression $ LocatedString "[1..]" initLocation in
                res `shouldBe` ListExpr [NumberConstExpr $ IntValue $ LocatedString "1" $ Location 1 2, ListRange]
        it "can parse complex expression" $
            let left = parseExpression $ LocatedString "buildUVs (transformVec4 pos pmatr.persp) $ swizzleByRepeatingToVec4 uv.xz" initLocation in
                left `shouldSatisfy` parsingSucceeded
    describe "parseScriptAttributes" $ do
        it "can parse \"@import Shader.Core\"" $
            let expect = Success ([ImportNode [LocatedString "Shader" $ Location 1 9, LocatedString "Core" $ Location 1 16]], LocatedString "" $ Location 1 20) in
                parseScriptAttributes (LocatedString "@import Shader.Core" initLocation) `shouldBe` expect
        it "can parse \"@[import Shader.Core]\"" $
            let expect = Success ([ImportNode ["Shader" <@> Location 1 10, "Core" <@> Location 1 17]], "" <@> Location 1 22) in
                parseScriptAttributes ("@[import Shader.Core]" <@> initLocation) `shouldBe` expect
        it "can parse \"@[import Shader.Core, import Quaternion]\"" $
            let expect = Success ([ImportNode ["Shader" <@> Location 1 10, "Core" <@> Location 1 17], ImportNode ["Quaternion" <@> Location 1 30]], "" <@> Location 1 41) in
                parseScriptAttributes ("@[import Shader.Core, import Quaternion]" <@> initLocation) `shouldBe` expect

parsingSucceeded :: ParseResult a -> Bool
parsingSucceeded (Success _) = True
parsingSucceeded _ = False
