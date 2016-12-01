module PSParser (parseNumber, parseIdentifier,
    NumberType(..), ExpressionNode(..), parseExpression, AttributeNode(..), parseScriptAttributes,
    PatternNode(..), parsePattern, TypeConstructionNode(..), parseType) where

import Data.Char (isLower, isUpper)
import Common
import ChainedParser

-- LocatedString operations --
next :: LocatedString -> LocatedString
next (('\n':xs) :@: loc) = xs :@: newLine loc
next ((_:xs) :@: loc) = xs :@: forward loc
next x@([] :@: _) = x
dropThenGo :: LocatedString -> ParseResult ()
dropThenGo = into . next

takeStrOpt :: (Char -> Bool) -> LocatedString -> (LocatedString, LocatedString)
takeStrOpt f (str :@: loc) = let (v, rest) = splitAt (countSatisfyElements f str) str in
    (v :@: loc, rest :@: forwardN (length v) loc)

countSatisfyElements :: (a -> Bool) -> [a] -> Int
countSatisfyElements f = impl 0 where
    impl n l = case l of
        (x:xs) | f x -> let nx = n + 1 in nx `seq` impl nx xs
        _ -> n

isPartOfIdentifier :: Char -> Bool
isPartOfIdentifier c = characterClass c `elem` [Other, Number]

-- Parsing primitives generating void --
dropSpaces' :: (a, LocatedString) -> ParseResult a      -- Allows new line
dropSpaces' (v, cs@('-':'-':c:_) :@: loc)
    | not $ c `charClassOf` Symbol = dropSpaces' (v, drop (nSpaces + 1) cs :@: (newLine . forwardN nSpaces) loc) where
        nSpaces = countSatisfyElements (/= '\n') cs
dropSpaces' (v, str@(c:_) :@: loc) | c `elem` " \t" = dropSpaces' (v, drop nSpaces str :@: forwardN nSpaces loc) where
    nSpaces = countSatisfyElements (`elem` " \t") str
dropSpaces' x = Success x
dropSpaces :: (a, LocatedString) -> ParseResult a
dropSpaces (v, str@(c:_) :@: loc) | c `elem` " \t" = dropSpaces (v, drop nSpaces str :@: forwardN nSpaces loc) where
    nSpaces = countSatisfyElements (`elem` " \t") str
dropSpaces x = Success x

ensureCharacter :: Char -> (a, LocatedString) -> ParseResult a
ensureCharacter c (v, (x:xs) :@: loc) | x == c = Success (v, xs :@: forward loc)
ensureCharacter _ (_, input) = Failed input

ignorePrevious :: (LocatedString -> ParseResult a) -> (b, LocatedString) -> ParseResult a
ignorePrevious f (_, r) = f r

-- Primitive Parsing --
data NumberType = IntValue LocatedString | FloatingValue LocatedString deriving Eq
instance Show NumberType where
    show (IntValue locs) = "IntValue(" ++ show locs ++ ")"
    show (FloatingValue locs) = "FloatingValue(" ++ show locs ++ ")"

parseNumber :: LocatedString -> ParseResult NumberType
parseNumber input@((c:_) :@: _) | c `charClassOf` Number = (Success $ takeStrOpt (`charClassOf` Number) input) /> parseMaybeFloating where
    parseMaybeFloating (v, rt@(('.':c2:_) :@: _)) | c2 `charClassOf` Number = entireStr `seq` FloatingValue <$> Success entireStr where
        entireStr = let (fpart, r) = takeStrOpt (`charClassOf` Number) $ next rt in ((v `append` ".") ~~ fpart, r)
    parseMaybeFloating (v, r) = Success (IntValue v, r)
parseNumber input = Failed input

parseIdentifier :: LocatedString -> ParseResult LocatedString
parseIdentifier input@((c:_) :@: _) | c `charClassOf` Other = Success $ takeStrOpt isPartOfIdentifier input
parseIdentifier input = Failed input

parseSymbolIdent :: LocatedString -> ParseResult LocatedString
parseSymbolIdent input@((c:_) :@: _) | c `charClassOf` Symbol = Success $ takeStrOpt (`charClassOf` Symbol) input
parseSymbolIdent input = Failed input

-- Script Attributes --
showsPrep :: Show a => String -> a -> String
showsPrep = flip (showParen True . const . show)
data AttributeNode = ImportNode [LocatedString] | VariableInNode ExpressionNode | VariableOutNode ExpressionNode |
    UniformBindNode { binding :: ExpressionNode, setIndex :: ExpressionNode } |
    InputAttachmentBindNode { binding :: ExpressionNode, setIndex :: ExpressionNode, index :: ExpressionNode } deriving Eq
instance Show AttributeNode where
    show (ImportNode path) = "ImportNode " ++ show path
    show (VariableInNode location) = "VariableInNode" `showsPrep` location
    show (VariableOutNode location) = "VariableOutNode" `showsPrep` location
    show (UniformBindNode b s) = "UniformBindNode binding=" `showsPrep` b ++ " set=" `showsPrep` s
    show (InputAttachmentBindNode b s i) = "InputAttachmentBindNode binding=" `showsPrep` b ++ " set=" `showsPrep` s ++ " index=" `showsPrep` i

parseScriptAttributes :: LocatedString -> ParseResult [AttributeNode]
parseScriptAttributes input@(('@':_) :@: _) = dropThenGo input /> dropSpaces /> ignorePrevious bracketOrSingle where
    bracketOrSingle r@(('[':_) :@: _) = parseElementsInBracket r
    bracketOrSingle r = (: []) <$> parseAttrElement r
    parseElementsInBracket r = dropThenGo r /> dropSpaces' /> ignorePrevious parseBracketNext
    parseBracketNext r@((']':_) :@: _) = Success ([], next r)
    parseBracketNext r = (: []) <$> parseAttrElement r /> dropSpaces /> parseElementsRecursive
    parseElementsRecursive (x, r@((',':_) :@: _)) = dropThenGo r /> dropSpaces' /> ignorePrevious parseAttrElement |=> (\e -> x ++ [e]) /> dropSpaces /> parseElementsRecursive
    parseElementsRecursive (x, r@((']':_) :@: _)) = Success (x, next r)
    parseElementsRecursive (_, r) = Failed r
parseScriptAttributes (_ :@: loc) = error $ "Parsing invalid string beginning from " ++ show loc
parseAttrElement :: LocatedString -> ParseResult AttributeNode
parseAttrElement input@(('i':'m':'p':'o':'r':'t':c:_) :@: _)
    | c `charClassOf` Ignore = into (iterate next input !! 6) /> dropSpaces /> ignorePrevious parseIdentifier |=> (: []) /> parsePathRec |=> ImportNode where
        parsePathRec (x, r@(('.':_) :@: _)) = dropThenGo r /> ignorePrevious parseIdentifier |=> (\i -> x ++ [i]) /> parsePathRec
        parsePathRec v = Success v
parseAttrElement input@(('i':'n':c:_) :@: _) | c `charClassOf` Ignore = into (iterate next input !! 2) /> dropSpaces /> ignorePrevious parseExpression |=> VariableInNode
parseAttrElement input@(('o':'u':'t':c:_) :@: _) | c `charClassOf` Ignore = into (iterate next input !! 3) /> dropSpaces /> ignorePrevious parseExpression |=> VariableOutNode
parseAttrElement input@(('u':'n':'i':'f':'o':'r':'m':c:_) :@: _) | c `charClassOf` Ignore = into (iterate next input !! 7) /> dropSpaces />
    ignorePrevious parseExpression /> dropSpaces /> (\(x, r) -> UniformBindNode x <$> parseExpression r)
parseAttrElement input@(('i':'n':'p':'u':'t':'A':'t':'t':'a':'c':'h':'m':'e':'n':'t':c:_) :@: _) | c `charClassOf` Ignore = into (iterate next input !! 15) /> dropSpaces />
    ignorePrevious parseExpression /> dropSpaces /> (\(x, r) -> (,) x <$> parseExpression r) /> dropSpaces /> (\((x, y), r) -> InputAttachmentBindNode x y <$> parseExpression r)
parseAttrElement input = Failed input

-- TypeNode --
data TypeConstructionNode =
    TypeNameNode LocatedString | TypeVariableNode LocatedString | TupleType [TypeConstructionNode] |
    FunctionDeriveTypeNode TypeConstructionNode TypeConstructionNode | TypeConApplyNode TypeConstructionNode TypeConstructionNode deriving Eq
instance Show TypeConstructionNode where
    show (TypeNameNode n) = "TypeName(" ++ show n ++ ")"
    show (TypeVariableNode n) = "TypeVariableNode("  ++ show n ++ ")"
    show (FunctionDeriveTypeNode l r) = "(" ++ show l ++ ")->(" ++ show r ++ ")"
    show (TypeConApplyNode f x) = show f ++ " " ++ show x
    show (TupleType xs) = "Tuple" ++ show xs

parseType :: LocatedString -> ParseResult TypeConstructionNode
parseType = parseFunctionDeriveType

parseFunctionDeriveType :: LocatedString -> ParseResult TypeConstructionNode
parseFunctionDeriveType input = parseTypeConApplying input /> dropSpaces /> parseFunctionDerivesRec where
    parseFunctionDerivesRec (x, r@(('-':'>':_) :@: _)) = into (iterate next r !! 2) /> dropSpaces /> ignorePrevious parseFunctionDeriveType |=> FunctionDeriveTypeNode x /> dropSpaces
    parseFunctionDerivesRec x = Success x

parseTypeConApplying :: LocatedString -> ParseResult TypeConstructionNode
parseTypeConApplying input = parseBasicType input /> dropSpaces /> parseApplyingRec where
    parseApplyingRec (x, r@((c:_) :@: _)) | c == '(' || c `charClassOf` Other = TypeConApplyNode x <$> parseBasicType r /> dropSpaces /> parseApplyingRec
    parseApplyingRec x = Success x

parseBasicType :: LocatedString -> ParseResult TypeConstructionNode
parseBasicType input@(('(':_) :@: _) = parseParenthesedType input
parseBasicType input@((l:_) :@: _)
    | l `charClassOf` Other = (if isUpper l then TypeNameNode else TypeVariableNode) <$> parseIdentifier input
parseBasicType input = Failed input

parseParenthesedType :: LocatedString -> ParseResult TypeConstructionNode
parseParenthesedType input = dropThenGo input /> dropSpaces /> ignorePrevious branchEmpty where
    branchEmpty r@((')':_) :@: _) = Success (TupleType [], next r)
    branchEmpty r = parseType r /> dropSpaces /> branchTuple
    branchTuple (x, r@((')':_) :@: _)) = Success (x, next r)
    branchTuple (x, r@((',':_) :@: _)) = Success ([x], next r) /> dropSpaces /> branchTerm
    branchTuple (_, r) = Failed r
    branchTerm (x, r@((')':_) :@: _)) = Success (TupleType x, next r)
    branchTerm (x, r) = (\e -> x ++ [e]) <$> parseType r /> dropSpaces /> branchNext
    branchNext (x, r@((')':_) :@: _)) = Success (TupleType x, next r)
    branchNext (x, r@((',':_) :@: _)) = Success (x, next r) /> dropSpaces /> branchTerm
    branchNext (_, r) = Failed r

-- PatternNode --
data PatternNode =
    IdentifierBindPat LocatedString | NumberConstPat NumberType | MemberRefPat [LocatedString] |
    SymbolIdentPat LocatedString | ListPat [PatternNode] | DataDecompositePat LocatedString [PatternNode] |
    BinaryPat PatternNode PatternNode PatternNode | AsPat LocatedString PatternNode | Wildcard | TuplePattern [PatternNode] deriving Eq
instance Show PatternNode where
    show (IdentifierBindPat loc) = "IdentifierBindPat " `showsPrep` loc
    show (NumberConstPat num) = "NumberConstPat " `showsPrep` num
    show (MemberRefPat mref) = "MemberRefPat " ++ show mref
    show (SymbolIdentPat sym) = "SymbolIdentPat " `showsPrep` sym
    show (ListPat exs) = "SymbolIdentPat " ++ show exs
    show (DataDecompositePat cons pats) = "DataDecompositePat " ++ show cons ++ " " ++ show pats
    show (BinaryPat left op right) = "BinaryPat " `showsPrep` left ++ " " `showsPrep` op ++ " " `showsPrep` right
    show (AsPat bindto pat) = show bindto ++ " as " `showsPrep` pat
    show Wildcard = "_"
    show (TuplePattern pats) = "Tuple" ++ show pats

parsePattern :: LocatedString -> ParseResult PatternNode
parsePattern = parseDecompositePattern

parseDecompositePattern :: LocatedString -> ParseResult PatternNode
parseDecompositePattern input@((c:_) :@: _) | isUpper c = parseIdentifier input /> dropSpaces />  (\(d, r) -> DataDecompositePat d <$> parseArgsRecursive ([], r)) where
    parseArgsRecursive (x, r) = (\p -> x ++ [p]) <$> parseAsPattern r /> dropSpaces /> parseArgsRecursive // const (Success (x, r))
parseDecompositePattern input = parseAsPattern input
parseAsPattern :: LocatedString -> ParseResult PatternNode
parseAsPattern input@((c:_) :@: _) | c `charClassOf` Other = parseIdentifier input /> dropSpaces /> (\(x, r) -> case r of
    ('@':_) :@: _ -> dropThenGo r /> dropSpaces /> (\(_, rt) -> case rt of
        (nc:_) :@: _ | isUpper nc -> AsPat x . (`DataDecompositePat` []) <$> parseIdentifier rt
        _ -> AsPat x <$> parsePatternPrimitives rt)
    _ -> parsePatternPrimitives input)
parseAsPattern input = parsePatternPrimitives input
parsePatternPrimitives :: LocatedString -> ParseResult PatternNode
parsePatternPrimitives input@(('_':c:_) :@: _) | not (c `charClassOf` Other || c `charClassOf` Number) && (c /= '_') = Success (Wildcard, next input)
parsePatternPrimitives input@(['_'] :@: _) = Success (Wildcard, next input)
parsePatternPrimitives input@((c:_) :@: _)
    | (c `charClassOf` Other) && isLower c = IdentifierBindPat <$> parseIdentifier input
    | c `charClassOf` Number = NumberConstPat <$> parseNumber input
parsePatternPrimitives input@(('(':_) :@: _) = parseParenthesedPattern input
parsePatternPrimitives input@(('[':_) :@: _) = parseListPattern input
parsePatternPrimitives input = Failed input
parseListPattern :: LocatedString -> ParseResult PatternNode
parseListPattern input = dropThenGo input /> dropSpaces /> ignorePrevious (\r -> ListPat <$> case r of
    (']':_) :@: _ -> Success ([], next r)
    _ -> (: []) <$> parsePattern r /> parsePatternListRec where
        parsePatternListRec (x, rt@((',':_) :@: _)) = dropThenGo rt /> dropSpaces /> ignorePrevious parsePattern |=> (\p -> x ++ [p]) /> parsePatternListRec
        parsePatternListRec (x, rt@((']':_) :@: _)) = Success (x, next rt)
        parsePatternListRec (_, rt) = Failed rt
    )
parseParenthesedPattern :: LocatedString -> ParseResult PatternNode
parseParenthesedPattern input = dropThenGo input /> dropSpaces /> branchNext where
    branchNext (_, r@((')':_) :@: _)) = Success (TuplePattern [], next r)
    branchNext (_, r) = parsePattern r /> dropSpaces /> branchNextAfterPat
    branchNextAfterPat (x, r@((')':_) :@: _)) = Success (x, next r)
    branchNextAfterPat (x, r@((',':_) :@: _)) = Success ([x], next r) /> dropSpaces /> branchInTuple
    branchNextAfterPat (_, r) = Failed r
    branchInTuple (x, r@((')':_) :@: _)) = Success (TuplePattern x, next r)
    branchInTuple (x, r) = (\e -> x ++ [e]) <$> parsePattern r /> dropSpaces /> branchInTupleCon
    branchInTupleCon (x, r@((')':_) :@: _)) = Success (TuplePattern x, next r)
    branchInTupleCon (x, r@((',':_) :@: _)) = Success (x, next r) /> dropSpaces /> branchInTuple
    branchInTupleCon (_, r) = Failed r 

-- Expression --
data ExpressionNode =
    IdentifierRefExpr LocatedString | NumberConstExpr NumberType | MemberRefExpr [LocatedString] |
    SymbolIdentExpr LocatedString | ListExpr [ExpressionNode] | FunApplyExpr ExpressionNode ExpressionNode |
    BinaryExpr ExpressionNode ExpressionNode ExpressionNode | ListRange | TupleExpr [ExpressionNode] deriving Eq
instance Show ExpressionNode where
    show (IdentifierRefExpr loc) = "IdentifierRefExpr " ++ show loc
    show (NumberConstExpr loc) = "NumberConstExpr " ++ show loc
    show (MemberRefExpr locs) = "MemberRefExpr" ++ show locs
    show (SymbolIdentExpr loc) = "SymbolIdentExpr " ++ show loc
    show (ListExpr exs) = "ListExpr" ++ show exs
    show (FunApplyExpr fx vx) = "FunApplyExpr(" ++ show fx ++ " " ++ show vx ++ ")"
    show (BinaryExpr l op r) = "BinaryExpr(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
    show ListRange = ".."
    show (TupleExpr exs) = "Tuple" ++ show exs

parseExpression, parseBinaryExpr, parseUnaryTerm, parsePrimaryTerm, parseFunctionCandidates :: LocatedString -> ParseResult ExpressionNode
parseMemberRefOrIdentRef, parseInfixOperator, parseListTerm, parseParenthesedExpressions :: LocatedString -> ParseResult ExpressionNode

parseExpression = parseBinaryExpr

parseBinaryExpr input = parseUnaryTerm input /> dropSpaces /> parseRecursive where
    parseRecursive (x, r) = parseInfixOperator r /> dropSpaces' /> (\(io, rest) -> BinaryExpr x io <$> parseUnaryTerm rest) /> dropSpaces /> parseRecursive // const (Success (x, r))

parseUnaryTerm input = parseFunctionCandidates input /> parseFunApplyArgsRec // const (parsePrimaryTerm input) where
    parseFunApplyArgsRec (x, r) = into r /> dropSpaces /> ignorePrevious parsePrimaryTerm |=> FunApplyExpr x /> parseFunApplyArgsRec // const (Success (x, r))

parsePrimaryTerm input@(('[':_) :@: _) = parseListTerm input
parsePrimaryTerm input@(('(':_) :@: _) = parseParenthesedExpressions input
parsePrimaryTerm input@((c:_) :@: _)
    | c `charClassOf` Number = NumberConstExpr <$> parseNumber input
    | c `charClassOf` Other = parseMemberRefOrIdentRef input
    | c `charClassOf` Symbol = SymbolIdentExpr <$> parseSymbolIdent input
parsePrimaryTerm input = Failed input

parseParenthesedExpressions input = dropThenGo input /> dropSpaces /> branchNext where
    branchNext (_, r@((')':_) :@: _)) = Success (TupleExpr [], next r)
    branchNext (_, r) = parseExpression r /> dropSpaces /> branchNextTup
    branchNextTup (x, r@((')':_) :@: _)) = Success (x, next r)
    branchNextTup (x, r@((',':_) :@: _)) = Success ([x], next r) /> dropSpaces /> branchTuple
    branchNextTup (_, r) = Failed r
    branchTuple (x, r@((')':_) :@: _)) = Success (TupleExpr x, next r)
    branchTuple (x, r) = (\e -> x ++ [e]) <$> parseExpression r /> dropSpaces /> branchTupleCont
    branchTupleCont (x, r@((')':_) :@: _)) = Success (TupleExpr x, next r)
    branchTupleCont (x, r@((',':_) :@: _)) = Success (x, next r) /> dropSpaces /> branchTuple
    branchTupleCont (_, r) = Failed r

parseFunctionCandidates input@(('(':_) :@: _) = dropThenGo input /> dropSpaces /> ignorePrevious parseExpression /> dropSpaces /> ensureCharacter ')'
parseFunctionCandidates input@((c:_) :@: _) | c `charClassOf` Other = parseMemberRefOrIdentRef input
parseFunctionCandidates input = parseInfixOperator input

parseMemberRefOrIdentRef input = (\mrefs -> if length mrefs == 1 then IdentifierRefExpr $ head mrefs else MemberRefExpr mrefs) <$> memberRefs where
    memberRefs = (: []) <$> parseIdentifier input /> parseMemberRefRecursive
    parseMemberRefRecursive (v, r@(('.':c:_) :@: _)) | c `charClassOf` Other = dropThenGo r /> ignorePrevious parseIdentifier |=> (\x -> v ++ [x]) /> parseMemberRefRecursive
    parseMemberRefRecursive x = Success x

parseInfixOperator input@(('`':c:_) :@: _)
    | c `charClassOf` Other = dropThenGo input /> ignorePrevious parseIdentifier |=> IdentifierRefExpr /> ensureCharacter '`'
parseInfixOperator input@((c:_) :@: _) | c `charClassOf` Symbol = SymbolIdentExpr <$> parseSymbolIdent input
parseInfixOperator input = Failed input

parseListTerm input@(('[':_) :@: _) = dropThenGo input /> dropSpaces /> ignorePrevious (\r -> ListExpr <$> case r of
    (']':_) :@: _ -> Success ([], next r)
    _ -> parseRangeOrExpression r /> parseExpressionListRec where
        parseExpressionListRec (x, rt@((',':_) :@: _)) = dropThenGo rt /> dropSpaces /> ignorePrevious parseRangeOrExpression |=> (++) x /> parseExpressionListRec
        parseExpressionListRec (x, rt@((']':_) :@: _)) = Success (x, next rt)
        parseExpressionListRec (_, rt) = Failed rt
    ) where
        parseRangeOrExpression r = parseExpression r /> dropSpaces /> (\(x, rt) -> case rt of
            ('.':'.':_) :@: _ -> into (iterate next rt !! 2) /> dropSpaces /> ignorePrevious (\res -> (\e -> x : ListRange : [e]) <$> parseExpression res // const (Success (x : [ListRange], res)))
            _ -> Success ([x], rt))
parseListTerm input = Failed input
