module PSParser (Location(Location), LocatedString(LocatedString), initLocation, ParseResult(Success, Failed), parseNumber, parseIdentifier,
    NumberType(..), ExpressionNode(..), parseExpression) where

isSpace :: Char -> Bool
isSpace c = c `elem` " \t"

data Location = Location { line :: Int, column :: Int } deriving Eq
instance Show Location where
    show loc = line_str ++ ":" ++ column_str where
        line_str = show $ line loc
        column_str = show $ column loc
-- A slice of string with its location on source
data LocatedString = LocatedString String Location deriving Eq
instance Show LocatedString where
    show (LocatedString str loc) = str ++ " at " ++ show loc
class Concatable a where
    (~~) :: a -> a -> a
instance Concatable LocatedString where
    (LocatedString sa l) ~~ (LocatedString sb _) = LocatedString (sa ++ sb) l
append :: LocatedString -> String -> LocatedString
append (LocatedString sa l) sb = LocatedString (sa ++ sb) l
-- The result of Parser
data ParseResult a = Success (a, LocatedString) | Failed LocatedString deriving Eq
instance Show a => Show (ParseResult a) where
    show (Success (v, r)) = "Success " ++ show v ++ " remains " ++ show r
    show (Failed r) = "Failed remaining " ++ show r
instance Functor ParseResult where
    fmap f (Success (v, r)) = Success (f v, r)
    fmap _ (Failed r) = Failed r
instance Applicative ParseResult where
    -- Lift a value with dummy remainings
    pure v = Success (v, LocatedString "" initLocation)
    (Success (f, _)) <*> (Success (v, r)) = Success (f v, r)
    -- as identity
    (Failed _) <*> (Success (v, r)) = Failed r
    _ <*> (Failed r) = Failed r
instance Monad ParseResult where
    (Success (v, r)) >>= f = f v
    (Failed r) >>= _ = Failed r
-- Lifts up into parser chain as remainings
into :: LocatedString -> ParseResult ()
into l = Success ((), l)

-- Continuous Parsing
(->>) :: ParseResult a -> ((a, LocatedString) -> ParseResult b) -> ParseResult b
infixl 2 ->>
(Success x) ->> f = f x
(Failed r) ->> _ = Failed r
-- Alternate Parsing
(//) :: ParseResult a -> (LocatedString -> ParseResult a) -> ParseResult a
infixl 1 //
r@(Success x) // _ = r
(Failed r) // f = f r

initLocation :: Location
initLocation = Location 1 1
forward :: Location -> Location
forward (Location l c) = Location l (c + 1)
forwardN :: Int -> Location -> Location
forwardN n (Location l c) = Location l (c + n)
newLine :: Location -> Location
newLine (Location l c) = Location (l + 1) 1

-- LocatedString operations --
next :: LocatedString -> LocatedString
next (LocatedString ('\n':xs) loc) = LocatedString xs $ newLine loc
next (LocatedString (_:xs) loc) = LocatedString xs $ forward loc

takeStrOpt :: (Char -> Bool) -> LocatedString -> (LocatedString, LocatedString)
takeStrOpt f (LocatedString str loc) = let (v, rest) = splitAt (countSatisfyElements f str) str in
    (LocatedString v loc, LocatedString rest (forwardN (length v) loc))

countSatisfyElements :: (a -> Bool) -> [a] -> Int
countSatisfyElements f = impl 0 where
    impl n l = case l of
        (x:xs) | f x -> let nx = n + 1 in nx `seq` impl nx xs
        _ -> n

-- Character Classifications --
data PairDirection = Open | Close deriving Eq
data CharacterClass = Ignore | InternalSymbol | LineFeed | Parenthese PairDirection | Bracket PairDirection | Symbol | Number | Other deriving Eq
determineCharacterClass :: Char -> CharacterClass
determineCharacterClass c
    | isSpace c = Ignore
    | c == '\n' = LineFeed
    | c == '(' = Parenthese Open
    | c == ')' = Parenthese Close
    | c == '[' = Bracket Open
    | c == ']' = Bracket Close
    | c `elem` "+-*/<>$?" = Symbol
    | c `elem` ".`" = InternalSymbol
    | '0' <= c && c <= '9' = Number
    | otherwise = Other

isPartOfIdentifier :: Char -> Bool
isPartOfIdentifier c = determineCharacterClass c `elem` [Other, Number]

-- Parsing primitives generating void --
dropSpaces :: (a, LocatedString) -> ParseResult a
ensureCharacter :: Char -> (a, LocatedString) -> ParseResult a
ignorePrevious :: (LocatedString -> ParseResult a) -> (b, LocatedString) -> ParseResult a

dropSpaces (v, input) = Success $ case input of
    LocatedString str@(c:_) loc | c `elem` " \t" -> let nSpaces = countSatisfyElements (`elem` " \t") str in (v, LocatedString (drop nSpaces str) (forwardN nSpaces loc))
    _ -> (v, input)

ensureCharacter c (v, LocatedString (x:xs) loc) | x == c = Success (v, LocatedString xs $ forward loc)
ensureCharacter _ (_, input) = Failed input

ignorePrevious f (_, r) = f r

-- Primitive Parsing --
data NumberType = IntValue LocatedString | FloatingValue LocatedString deriving Eq
instance Show NumberType where
    show (IntValue locs) = "IntValue(" ++ show locs ++ ")"
    show (FloatingValue locs) = "FloatingValue(" ++ show locs ++ ")"

parseNumber :: LocatedString -> ParseResult NumberType
parseNumber input@(LocatedString (c:_) _) | '0' <= c && c <= '9' = (Success $ takeStrOpt (\x -> determineCharacterClass x == Number) input) ->> parseMaybeFloating where
    parseMaybeFloating (v, input@(LocatedString ('.':c:_) _)) | '0' <= c && c <= '9' = entireStr `seq` FloatingValue <$> Success entireStr where
        entireStr = let (fpart, r) = takeStrOpt (\x -> determineCharacterClass x == Number) $ next input in ((v `append` ".") ~~ fpart, r)
    parseMaybeFloating (v, r) = Success (IntValue v, r)
parseNumber input = Failed input

parseIdentifier :: LocatedString -> ParseResult LocatedString
parseIdentifier input = case input of
    LocatedString (c:_) _ | determineCharacterClass c == Other -> Success $ takeStrOpt (\x -> determineCharacterClass x `elem` [Other, Number]) input
    _ -> Failed input

parseSymbolIdent :: LocatedString -> ParseResult LocatedString
parseSymbolIdent input = case input of
    LocatedString (c:_) _ | determineCharacterClass c == Symbol -> Success $ takeStrOpt (\x -> determineCharacterClass x == Symbol) input
    _ -> Failed input

-- Expression --
data ExpressionNode = IdentifierRefExpr LocatedString | NumberConstExpr NumberType | MemberRefExpr [LocatedString] |
    SymbolIdentExpr LocatedString | ListExpr [ExpressionNode] | NegativeOpExpr ExpressionNode | InvertOpExpr ExpressionNode |
    FunApplyExpr ExpressionNode ExpressionNode | BinaryExpr ExpressionNode ExpressionNode ExpressionNode deriving Eq
instance Show ExpressionNode where
    show (IdentifierRefExpr loc) = "IdentifierRefExpr " ++ show loc
    show (NumberConstExpr loc) = "NumberConstExpr " ++ show loc
    show (MemberRefExpr locs) = "MemberRefExpr" ++ show locs
    show (SymbolIdentExpr loc) = "SymbolIdentExpr " ++ show loc
    show (ListExpr exs) = "ListExpr" ++ show exs
    show (NegativeOpExpr expr) = "NegativeOp(" ++ show expr ++ ")"
    show (InvertOpExpr expr) = "InvertOp(" ++ show expr ++ ")"
    show (FunApplyExpr fx vx) = "FunApplyExpr(" ++ show fx ++ " " ++ show vx ++ ")"
    show (BinaryExpr l op r) = "BinaryExpr(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"

parseExpression :: LocatedString -> ParseResult ExpressionNode
parseExpression = parseBinaryExpr

parseBinaryExpr :: LocatedString -> ParseResult ExpressionNode
parseBinaryExpr input = parseUnaryTerm input ->> dropSpaces ->> parseRecursive where
    parseRecursive (x, r) = parseInfixOperators r ->> dropSpaces ->> (\(io, rest) -> BinaryExpr x io <$> parseUnaryTerm rest) ->> dropSpaces ->> parseRecursive // const (Success (x, r))

parseInfixOperators :: LocatedString -> ParseResult ExpressionNode
parseInfixOperators input@(LocatedString (c:_) _)
    | c == '`' = into (next input) ->> dropSpaces ->> ignorePrevious (\r -> IdentifierRefExpr <$> parseIdentifier r) ->> dropSpaces ->> ensureCharacter '`'
    | determineCharacterClass c == Symbol = SymbolIdentExpr <$> parseSymbolIdent input
parseInfixOperators input = Failed input

parseUnaryTerm :: LocatedString -> ParseResult ExpressionNode
parseUnaryTerm input@(LocatedString ('-':_) _) = NegativeOpExpr <$> (into (next input) ->> dropSpaces ->> ignorePrevious parseUnaryTerm)
parseUnaryTerm input@(LocatedString ('~':_) _) = InvertOpExpr <$> (into (next input) ->> dropSpaces ->> ignorePrevious parseUnaryTerm)
parseUnaryTerm input = parseFunctionCandidates input ->> parseFunApplyArgsRec // const (parsePrimaryTerm input) where
    parseFunApplyArgsRec (x, input@(LocatedString (c:_) _))
        | c `elem` " \t" = into input ->> dropSpaces ->> (\(_, r) -> FunApplyExpr x <$> parsePrimaryTerm r) ->> parseFunApplyArgsRec
    parseFunApplyArgsRec v = Success v

parsePrimaryTerm :: LocatedString -> ParseResult ExpressionNode
parsePrimaryTerm input@(LocatedString (c:_) _) = case determineCharacterClass c of
    Number -> NumberConstExpr <$> parseNumber input
    Bracket Open -> parseListTerm input
    _ -> parseFunctionCandidates input
parsePrimaryTerm input = Failed input

parseFunctionCandidates :: LocatedString -> ParseResult ExpressionNode
parseFunctionCandidates input@(LocatedString (c:_) _) = case determineCharacterClass c of
    Parenthese Open -> into (next input) ->> dropSpaces ->> ignorePrevious parseExpression ->> dropSpaces ->> ensureCharacter ')'
    Symbol -> SymbolIdentExpr <$> parseSymbolIdent input
    Other -> (\memberRefs -> if length memberRefs == 1 then IdentifierRefExpr . head $ memberRefs else MemberRefExpr memberRefs) <$> memberRefs where
        memberRefs = pure <$> parseIdentifier input ->> parseMemberRefCont
        parseMemberRefCont (v, input) = case input of
            LocatedString ('.':xs) _ -> into (next input) ->> dropSpaces ->> (\(_, r) -> (\x -> v ++ [x]) <$> parseIdentifier r) ->> parseMemberRefCont
            _ -> Success (v, input)
    _ -> Failed input
parseFunctionCandidates input = Failed input

parseListTerm :: LocatedString -> ParseResult ExpressionNode
parseListTerm input@(LocatedString ('[':_) _) = into (next input) ->> dropSpaces ->> ignorePrevious (\r -> ListExpr <$> case r of
    LocatedString (']':_) _ -> Success ([], next r)
    _ -> (: []) <$> parseExpression r ->> dropSpaces ->> parseExpressionListRec where
        parseExpressionListRec (x, r@(LocatedString (',':_) _)) = into (next r) ->> dropSpaces ->> ignorePrevious (\r -> (\e -> x ++ [e]) <$> parseExpression r) ->> dropSpaces ->> parseExpressionListRec
        parseExpressionListRec (x, r@(LocatedString (']':_) _)) = Success (x, next r)
        parseExpressionListRec (_, r) = Failed r
    )
parseListTerm input = Failed input
