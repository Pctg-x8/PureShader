import Test.Hspec
import Test.Hspec.Expectations ()
import PSParser

main :: IO ()
main = hspec $ do
    describe "dropComments" $
        it "drops line comments exactly" $
            parseExpression ("--line_comment\napp 3 $ 3" :@: initLocation) `shouldSatisfy` parsingSucceeded
    describe "parseNumber" $ do
        it "returns Success with \"0123\" :@: initLocation" $
            parseNumber ("0123" :@: initLocation) `shouldBe` Success (IntValue $ "0123" :@: initLocation, "" :@: Location 1 5)
        it "returns Failed and remains entire of input" $
            parseNumber ("abc" :@: initLocation) `shouldBe` Failed ("abc" :@: initLocation)
    describe "parseIdentifier" $ do
        it "returns Success with \"abc\" :@: initLocation" $
            parseIdentifier ("abc " :@: initLocation) `shouldBe` Success ("abc" :@: initLocation, " " :@: Location 1 4)
        it "returns Failed and remains entire of input" $
            parseIdentifier (" abc" :@: initLocation) `shouldBe` Failed (" abc" :@: initLocation)
        it "can parse that contains characters out of ascii" $
            parseIdentifier ("にゃーん" :@: initLocation) `shouldBe` Success ("にゃーん" :@: initLocation, "" :@: Location 1 5)
    describe "parseExpression" $ do
        it "can parse \"にゃあ\"" $
            let Success (result, _) = parseExpression ("にゃあ\n" :@: initLocation) in result `shouldBe` (IdentifierRefExpr $ "にゃあ" :@: initLocation)
        it "can parse \"(3)\" and returns same value as 3" $
            parseExpression ("( 3)" :@: initLocation) `shouldBe` Success (NumberConstExpr $ IntValue ("3" :@: Location 1 3), "" :@: Location 1 5)
        it "can parse \"vin.xyz\"" $ let Success (result, _) = parseExpression ("vin.xyz" :@: initLocation) in
            result `shouldBe` MemberRefExpr ["vin" :@: initLocation, "xyz" :@: Location 1 5]
        it "can parse \"-3\"" $ let Success (result, _) = parseExpression $ "-3" :@: initLocation in
            result `shouldBe` FunApplyExpr (SymbolIdentExpr $ "-" :@: initLocation) (NumberConstExpr $ IntValue ("3" :@: Location 1 2))
        it "can parse \"~ 0.0\"" $ let Success (result, _) = parseExpression $ "~ 0.0" :@: initLocation in
            result `shouldBe` FunApplyExpr (SymbolIdentExpr $ "~" :@: initLocation) (NumberConstExpr $ FloatingValue ("0.0" :@: Location 1 3))
        it "can parse \"Vec4 0 0 0 1\"" $ let
            expect = FunApplyExpr (FunApplyExpr (FunApplyExpr (FunApplyExpr name first) second) third) fourth
            name = IdentifierRefExpr $ "Vec4" :@: initLocation
            first = NumberConstExpr $ IntValue $ "0" :@: Location 1 6
            second = NumberConstExpr $ IntValue $ "0" :@: Location 1 8
            third = NumberConstExpr $ IntValue $ "0" :@: Location 1 10
            fourth = NumberConstExpr $ IntValue $ "1" :@: Location 1 12
            Success (result, _) = parseExpression $ "Vec4 0 0 0 1" :@: initLocation in
                result `shouldBe` expect
        it "can parse \"3 3\" and returns the result which is same as \"3\"" $ let
            Success (res1, _) = parseExpression $ "3 3" :@: initLocation
            Success (res2, _) = parseExpression $ "3" :@: initLocation in
                res1 `shouldBe` res2
        it "can parse \"2 + 3\"" $ let
            Success (res, _) = parseExpression $ "2 + 3" :@: initLocation
            l = NumberConstExpr $ IntValue $ "2" :@: initLocation
            op = SymbolIdentExpr $ "+" :@: Location 1 3
            r = NumberConstExpr $ IntValue $ "3" :@: Location 1 5 in
                res `shouldBe` BinaryExpr l op r
        it "can parse \"2 `elem` arrayList\"" $ let
            Success (res, _) = parseExpression $ "2 `elem` arrayList" :@: initLocation
            l = NumberConstExpr $ IntValue $ "2" :@: initLocation
            op = IdentifierRefExpr $ "elem" :@: Location 1 4
            r = IdentifierRefExpr $ "arrayList" :@: Location 1 10 in
                res `shouldBe` BinaryExpr l op r
        it "can parse \"2 `elem` [2, 3, 4]\"" $ let
            Success (res, _) = parseExpression $ "2 `elem` [2, 3, 4]" :@: initLocation
            l = NumberConstExpr $ IntValue $ "2" :@: initLocation
            op = IdentifierRefExpr $ "elem" :@: Location 1 4
            r = ListExpr [NumberConstExpr $ IntValue $ "2" :@: Location 1 11, NumberConstExpr $ IntValue $ "3" :@: Location 1 14, NumberConstExpr $ IntValue $ "4" :@: Location 1 17] in
                res `shouldBe` BinaryExpr l op r
        it "can parse \"[1, 3 .. 10, 11]\"" $ let
            Success (res, _) = parseExpression $ "[1, 3 .. 10, 11]" :@: initLocation
            first = NumberConstExpr $ IntValue $ "1" :@: Location 1 2
            second = NumberConstExpr $ IntValue $ "3" :@: Location 1 5
            third = NumberConstExpr $ IntValue $ "10" :@: Location 1 10
            fourth = NumberConstExpr $ IntValue $ "11" :@: Location 1 14 in
                res `shouldBe` ListExpr [first, second, ListRange, third, fourth]
        it "can parse \"[1..]\"" $ let Success (res, _) = parseExpression $ "[1..]" :@: initLocation in
            res `shouldBe` ListExpr [NumberConstExpr $ IntValue $ "1" :@: Location 1 2, ListRange]
        it "can parse complex expression" $ let left = parseExpression $ "buildUVs (transformVec4 pos pmatr.persp) $ swizzleByRepeatingToVec4 uv.xz" :@: initLocation in
            left `shouldSatisfy` parsingSucceeded
    describe "parseScriptAttributes" $
        (let expect = Success ([ImportNode ["Shader" :@: Location 1 9, "Core" :@: Location 1 16]], "" :@: Location 1 20) in
            it "can parse \"@import Shader.Core\"" $ parseScriptAttributes ("@import Shader.Core" :@: initLocation) `shouldBe` expect) >>
        (let expect = Success ([ImportNode ["Shader" :@: Location 1 10, "Core" :@: Location 1 17]], "" :@: Location 1 22) in
            it "can parse \"@[import Shader.Core]\"" $ parseScriptAttributes ("@[import Shader.Core]" :@: initLocation) `shouldBe` expect) >>
        (let expect = Success ([ImportNode ["Shader" :@: Location 1 10, "Core" :@: Location 1 17], ImportNode ["Quaternion" :@: Location 1 30]], "" :@: Location 1 41) in
            it "can parse \"@[import Shader.Core, import Quaternion]\"" $ parseScriptAttributes ("@[import Shader.Core, import Quaternion]" :@: initLocation) `shouldBe` expect) >>
        (let expect = Success ([VariableInNode $ NumberConstExpr $ IntValue $ "0" :@: Location 1 5], "" :@: Location 1 6) in
            it "can parse \"@in 0\"" $ parseScriptAttributes ("@in 0" :@: initLocation) `shouldBe` expect) >>
        (let expect = Success ([VariableOutNode $ IdentifierRefExpr $ "ovIndex" :@: Location 1 6], "" :@: Location 1 13) in
            it "can parse \"@out ovIndex\"" $ parseScriptAttributes ("@out ovIndex" :@: initLocation) `shouldBe` expect) >>
        (let expect = Success ([UniformBindNode (NumberConstExpr . IntValue $ "0" :@: Location 1 10) (NumberConstExpr . IntValue $ "0" :@: Location 1 12)], "" :@: Location 1 13) in
            it "can parse \"@uniform 0 0\"" $ parseScriptAttributes ("@uniform 0 0" :@: initLocation) `shouldBe` expect)
    describe "parsePattern" (
        (let expect = Success (IdentifierBindPat $ "x" :@: initLocation, "" :@: Location 1 2) in
            it "can parse \"x\" as bind pattern" $ parsePattern ("x" :@: initLocation) `shouldBe` expect) >>
        (let expect = Success (AsPat ("input" :@: initLocation) Wildcard, "" :@: Location 1 10) in
            it "can parse \"input@(_)\"" $ parsePattern ("input@(_)" :@: initLocation) `shouldBe` expect) >>
        (let expect = Success (AsPat ("input" :@: initLocation) (DataDecompositePat ("Terminal" :@: Location 1 7) []), "" :@: Location 1 15) in
            it "can parse \"input@Terminal\"" $ parsePattern ("input@Terminal" :@: initLocation) `shouldBe` expect) >>
        (let expect = Success (DataDecompositePat ("Vec4" :@: initLocation) [IdentifierBindPat ("x" :@: Location 1 6), AsPat ("y" :@: Location 1 8) (NumberConstPat $ IntValue $ "0" :@: Location 1 12), Wildcard, Wildcard], "" :@: Location 1 17) in
            it "can parse \"Vec4 x y@0 _ _\"" $ parsePattern ("Vec4 x y @ 0 _ _" :@: initLocation) `shouldBe` expect) >>
        (let expect = Success (DataDecompositePat ("ExprList" :@: initLocation) [AsPat ("bin" :@: Location 1 10) (ListPat [IdentifierBindPat $ "t" :@: Location 1 15, NumberConstPat $ IntValue $ "10" :@: Location 1 18])], "" :@: Location 1 21) in
            it "can parse \"ExprList bin@[t, 10]\"" $ parsePattern ("ExprList bin@[t, 10]" :@: initLocation) `shouldBe` expect))
    describe "parseType" (
        (let expect = TypeNameNode $ "Located" :@: initLocation; Success (r, _) = parseType $ "Located" :@: initLocation in
            it "can parse \"Located\" as TypeName" $ r `shouldBe` expect) >>
        (let expect = TypeVariableNode $ "a" :@: initLocation; Success (r, _) = parseType $ "a" :@: initLocation in
            it "can parse \"a\" as TypeVariable" $ r `shouldBe` expect) >>
        (let expect = FunctionDeriveTypeNode (TypeVariableNode $ "a" :@: initLocation) $ FunctionDeriveTypeNode (TypeVariableNode $ "a" :@: Location 1 6) (TypeNameNode $ "Int" :@: Location 1 11)
             Success (r, _) = parseType $ "a -> a -> Int" :@: initLocation in
            it "can parse \"a -> a -> Int\"" $ r `shouldBe` expect) >>
        (let Success (r2, _) = parseType $ "a -> (a -> Int)" :@: initLocation
             Success (r, _) = parseType $ "a -> a -> Int" :@: initLocation in
            it "can parse \"a -> a -> Int\" same as \"a -> (a -> Int)\"" $ simplifyTypeSyntaxTree r `shouldBe` simplifyTypeSyntaxTree r2) >>
        (let f1 = FunctionDeriveTypeNode (TypeVariableNode $ "a" :@: Location 1 2) (TypeVariableNode $ "b" :@: Location 1 7)
             f2 = FunctionDeriveTypeNode (TypeVariableNode $ "b" :@: Location 1 14) (TypeNameNode $ "Int" :@: Location 1 19)
             expect = FunctionDeriveTypeNode f1 (FunctionDeriveTypeNode f2 (FunctionDeriveTypeNode (TypeVariableNode $ "a" :@: Location 1 27) (TypeNameNode $ "Int" :@: Location 1 32)))
             Success (r, _) = parseType $ "(a -> b) -> (b -> Int) -> a -> Int" :@: initLocation in
            it "can parse \"(a -> b) -> (b -> Int) -> a -> Int\"" $ r `shouldBe` expect))

parsingSucceeded :: ParseResult a -> Bool
parsingSucceeded (Success _) = True
parsingSucceeded _ = False

simplifyTypeSyntaxTree :: TypeConstructionNode -> TypeConstructionNode
simplifyTypeSyntaxTree (TypeNameNode (a :@: _)) = TypeNameNode (a :@: initLocation)
simplifyTypeSyntaxTree (TypeVariableNode (a :@: _)) = TypeVariableNode (a :@: initLocation)
simplifyTypeSyntaxTree (FunctionDeriveTypeNode a b) = FunctionDeriveTypeNode (simplifyTypeSyntaxTree a) (simplifyTypeSyntaxTree b)
