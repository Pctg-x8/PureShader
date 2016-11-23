import Test.Hspec
import Test.Hspec.Expectations
import PSParser

main :: IO ()
main = hspec $ do
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
                result `shouldBe` NegativeOpExpr (NumberConstExpr $ IntValue (LocatedString "3" $ Location 1 2))
        it "can parse \"~ 0.0\"" $
            let Success (result, _) = parseExpression $ LocatedString "~ 0.0" initLocation in
                result `shouldBe` InvertOpExpr (NumberConstExpr $ FloatingValue (LocatedString "0.0" $ Location 1 3))
        it "can parse \"Vec4 0 0 0 1\"" $
            let Success (result, _) = parseExpression $ LocatedString "Vec4 0 0 0 1" initLocation in
                result `shouldBe` FunApplyExpr (IdentifierRefExpr $ LocatedString "Vec4" initLocation) [cz $ Location 1 6, cz $ Location 1 8, cz $ Location 1 10, co $ Location 1 12] where 
                    cz l = NumberConstExpr $ IntValue (LocatedString "0" l)
                    co l = NumberConstExpr $ IntValue (LocatedString "1" l)
