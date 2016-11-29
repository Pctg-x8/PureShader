{-# LANGUAGE TypeOperators #-}

module PSParser (Location(Location), LocatedString(..), initLocation, ParseResult(Success, Failed), parseNumber, parseIdentifier,
    NumberType(..), ExpressionNode(..), parseExpression, AttributeNode(..), parseScriptAttributes) where

isSpace :: Char -> Bool
isSpace c = c `elem` " \t"

data Location = Location { line :: Int, column :: Int } deriving Eq
instance Show Location where
    show loc = line_str ++ ":" ++ column_str where
        line_str = show $ line loc
        column_str = show $ column loc
-- A slice of string with its location on source
data LocatedString = String :@: Location deriving Eq
instance Show LocatedString where
    show (str :@: loc) = str ++ " at " ++ show loc
class Concatable a where
    (~~) :: a -> a -> a
instance Concatable LocatedString where
    (sa :@: l) ~~ (sb :@: _) = (sa ++ sb) :@: l
append :: LocatedString -> String -> LocatedString
append (sa :@: l) sb = (sa ++ sb) :@: l
-- The result of Parser
data ParseResult a = Success (a, LocatedString) | Failed LocatedString deriving Eq
instance Show a => Show (ParseResult a) where
    show (Success (v, r)) = "Success " ++ show v ++ " remains " ++ show r
    show (Failed r) = "Failed remaining " ++ show r
instance Functor ParseResult where
    fmap f (Success (v, r)) = Success (f v, r)
    fmap _ (Failed r) = Failed r
-- Lifts up into parser chain as remainings
into :: LocatedString -> ParseResult ()
into l = Success ((), l)

-- Continuous Parsing
(/>) :: ParseResult a -> ((a, LocatedString) -> ParseResult b) -> ParseResult b
infixl 2 />
(Success x) /> f = f x
(Failed r) /> _ = Failed r
-- andThen = (/>)
-- Alternate Parsing
(//) :: ParseResult a -> (LocatedString -> ParseResult a) -> ParseResult a
infixl 1 //
r@(Success _) // _ = r
(Failed r) // f = f r
-- orElse = (//)
-- Reducing a value
(|=>) :: ((c, LocatedString) -> ParseResult a) -> (a -> b) -> (c, LocatedString) -> ParseResult b
infixl 3 |=>
(|=>) partialfunc reducer x = reducer <$> partialfunc x
-- reduce = (|=>)

initLocation :: Location
initLocation = Location 1 1
forward :: Location -> Location
forward (Location l c) = Location l (c + 1)
forwardN :: Int -> Location -> Location
forwardN n (Location l c) = Location l (c + n)
newLine :: Location -> Location
newLine (Location l _) = Location (l + 1) 1

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

-- Character Classifications --
data PairDirection = Open | Close deriving Eq
data CharacterClass = Ignore | InternalSymbol | LineFeed | Parenthese PairDirection | Bracket PairDirection | Symbol | Number | Other deriving Eq
characterClass :: Char -> CharacterClass
characterClass c
    | isSpace c = Ignore
    | c == '\n' = LineFeed
    | c == '(' = Parenthese Open
    | c == ')' = Parenthese Close
    | c == '[' = Bracket Open
    | c == ']' = Bracket Close
    | c `elem` "+-*/<>$?~@" = Symbol
    | c `elem` ".`," = InternalSymbol
    | c `elem` ['0'..'9'] = Number
    | otherwise = Other
charClassOf :: Char -> CharacterClass -> Bool
charClassOf c t = characterClass c == t

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
parseScriptAttributes input@(('@':_) :@: _) = dropThenGo input /> dropSpaces /> ignorePrevious (\r -> case r of
    ('[':_) :@: _ -> parseElementsInBracket r
    _ -> (: []) <$> parseAttrElement r) where
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

-- Expression --
data ExpressionNode =
    IdentifierRefExpr LocatedString | NumberConstExpr NumberType | MemberRefExpr [LocatedString] |
    SymbolIdentExpr LocatedString | ListExpr [ExpressionNode] | FunApplyExpr ExpressionNode ExpressionNode |
    BinaryExpr ExpressionNode ExpressionNode ExpressionNode | ListRange deriving Eq
instance Show ExpressionNode where
    show (IdentifierRefExpr loc) = "IdentifierRefExpr " ++ show loc
    show (NumberConstExpr loc) = "NumberConstExpr " ++ show loc
    show (MemberRefExpr locs) = "MemberRefExpr" ++ show locs
    show (SymbolIdentExpr loc) = "SymbolIdentExpr " ++ show loc
    show (ListExpr exs) = "ListExpr" ++ show exs
    show (FunApplyExpr fx vx) = "FunApplyExpr(" ++ show fx ++ " " ++ show vx ++ ")"
    show (BinaryExpr l op r) = "BinaryExpr(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
    show ListRange = ".."

parseExpression, parseBinaryExpr, parseUnaryTerm, parsePrimaryTerm, parseFunctionCandidates :: LocatedString -> ParseResult ExpressionNode
parseMemberRefOrIdentRef, parseInfixOperator, parseListTerm :: LocatedString -> ParseResult ExpressionNode

parseExpression = parseBinaryExpr

parseBinaryExpr input = parseUnaryTerm input /> dropSpaces /> parseRecursive where
    parseRecursive (x, r) = parseInfixOperator r /> dropSpaces' /> (\(io, rest) -> BinaryExpr x io <$> parseUnaryTerm rest) /> dropSpaces /> parseRecursive // const (Success (x, r))

parseUnaryTerm input = parseFunctionCandidates input /> parseFunApplyArgsRec // const (parsePrimaryTerm input) where
    parseFunApplyArgsRec (x, r) = into r /> dropSpaces /> ignorePrevious parsePrimaryTerm |=> FunApplyExpr x /> parseFunApplyArgsRec // const (Success (x, r))

parsePrimaryTerm input@(('[':_) :@: _) = parseListTerm input
parsePrimaryTerm input@(('(':_) :@: _) = dropThenGo input /> dropSpaces' /> ignorePrevious parseExpression /> dropSpaces' /> ensureCharacter ')'
parsePrimaryTerm input@((c:_) :@: _)
    | c `charClassOf` Number = NumberConstExpr <$> parseNumber input
    | c `charClassOf` Other = parseMemberRefOrIdentRef input
    | c `charClassOf` Symbol = SymbolIdentExpr <$> parseSymbolIdent input
parsePrimaryTerm input = Failed input

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
