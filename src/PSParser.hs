module PSParser (Location(Location), LocatedString(LocatedString), initLocation, ParseResult(Success, Failed), parseNumber, parseIdentifier,
    NumberType(..), ExpressionNode(..), parseExpression, AttributeNode(..), parseScriptAttributes) where

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

data PairDirection = Open | Close deriving Eq
data CharacterClass = Ignore | InternalSymbol | LineFeed | Parenthese PairDirection | Symbol | Number | Other deriving Eq
determineCharacterClass :: Char -> CharacterClass
determineCharacterClass c
    | isSpace c = Ignore
    | c == '\n' = LineFeed
    | c == '(' = Parenthese Open
    | c == ')' = Parenthese Close
    | c `elem` "+-*/<>$?" = Symbol
    | c `elem` "." = InternalSymbol
    | '0' <= c && c <= '9' = Number
    | otherwise = Other

dropSpaces :: (a, LocatedString) -> ParseResult a
dropSpaces (v, input) = Success $ case input of
    LocatedString str@(c:_) loc | c `elem` " \t" -> let nSpaces = countSatisfyElements (`elem` " \t") str in (v, LocatedString (drop nSpaces str) (forwardN nSpaces loc))
    _ -> (v, input)

ensureCharacter :: Char -> (a, LocatedString) -> ParseResult a
ensureCharacter c (v, input) = case input of
    LocatedString (x:xs) loc | x == c -> Success (v, LocatedString xs $ forward loc)
    _ -> Failed input
ignorePrevious :: (LocatedString -> ParseResult a) -> (b, LocatedString) -> ParseResult a
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

-- Script Attributes --
data AttributeNode = ImportNode [LocatedString] deriving Eq
instance Show AttributeNode where
    show (ImportNode path) = "ImportNode " ++ show path

parseScriptAttributes :: LocatedString -> ParseResult [AttributeNode]
parseScriptAttributes input@(LocatedString ('@':_) _) = into (next input) ->> dropSpaces ->> ignorePrevious (\r -> case r of
    LocatedString ('[':_) _ -> into (next input) ->> dropSpaces ->> ignorePrevious (\r -> parseElementsRecursive ([], r)) where
        parseElementsRecursive (x, r@(LocatedString (',':_) _)) = into (next r) ->> dropSpaces ->> ignorePrevious (\r -> (\e -> x ++ [e]) <$> parseElement r) ->> dropSpaces ->> parseElementsRecursive
        parseElementsRecursive (x, r@(LocatedString (']':_) _)) = Success (x, next r)
        parseElementsRecursive (_, input) = Failed input
    _ -> (: []) <$> parseElement r
    ) where
        parseElement input@(LocatedString ('i':'m':'p':'o':'r':'t':c:_) _) | determineCharacterClass c == Ignore = parseImport input
        parseElement input = Failed input
parseImport :: LocatedString -> ParseResult AttributeNode
parseImport input@(LocatedString ('i':'m':'p':'o':'r':'t':c:_) _)
    | determineCharacterClass c == Ignore = into (iterate next input !! 6) ->> dropSpaces ->> ignorePrevious (\r -> (: []) <$> parseIdentifier r) ->> (\x -> ImportNode <$> parsePathRec x) where
        parsePathRec (x, r@(LocatedString ('.':_) _)) = into (next r) ->> ignorePrevious (\r -> (\i -> x ++ [i]) <$> parseIdentifier r) ->> parsePathRec
        parsePathRec v = Success v

-- Expression --
data ExpressionNode = IdentifierRefExpr LocatedString | NumberConstExpr NumberType | MemberRefExpr [LocatedString] |
    NegativeOpExpr ExpressionNode | InvertOpExpr ExpressionNode | FunApplyExpr ExpressionNode [ExpressionNode] deriving Eq
instance Show ExpressionNode where
    show (IdentifierRefExpr loc) = "IdentifierRefExpr " ++ show loc
    show (NumberConstExpr loc) = "NumberConstExpr " ++ show loc
    show (MemberRefExpr locs) = "MemberRefExpr" ++ show locs
    show (NegativeOpExpr expr) = "NegativeOp(" ++ show expr ++ ")"
    show (InvertOpExpr expr) = "InvertOp(" ++ show expr ++ ")"
    show (FunApplyExpr fx vx) = "FunApplyExpr(" ++ show fx ++ " " ++ show vx ++ ")"

parseExpression :: LocatedString -> ParseResult ExpressionNode
parseExpression = parseUnaryTerm

parseUnaryTerm :: LocatedString -> ParseResult ExpressionNode
parseUnaryTerm input@(LocatedString ('-':_) _) = NegativeOpExpr <$> (into (next input) ->> dropSpaces ->> ignorePrevious parseUnaryTerm)
parseUnaryTerm input@(LocatedString ('~':_) _) = InvertOpExpr <$> (into (next input) ->> dropSpaces ->> ignorePrevious parseUnaryTerm)
parseUnaryTerm input = parsePrimaryTerm input ->> (\(f, r) -> (\ret -> if null ret then f else FunApplyExpr f ret) <$> parseFunApplyArgsRec ([], r)) // (\_ -> parsePrimaryTerm input) where
    parseFunApplyArgsRec (x, input@(LocatedString (c:_) _))
        | c `elem` " \t" = into input ->> dropSpaces ->> (\(_, r) -> (\d -> x ++ [d]) <$> parsePrimaryTerm r) ->> parseFunApplyArgsRec
    parseFunApplyArgsRec v = Success v

parsePrimaryTerm :: LocatedString -> ParseResult ExpressionNode
parsePrimaryTerm input@(LocatedString (c:_) _) = case determineCharacterClass c of
    Number -> NumberConstExpr <$> parseNumber input
    _ -> parseFunctionCandidates input
parsePrimaryTerm input = Failed input

parseFunctionCandidates :: LocatedString -> ParseResult ExpressionNode
parseFunctionCandidates input@(LocatedString (c:_) _) = case determineCharacterClass c of
    Parenthese Open -> into (next input) ->> dropSpaces ->> ignorePrevious parseExpression ->> dropSpaces ->> ensureCharacter ')'
    Other -> (\memberRefs -> if length memberRefs == 1 then IdentifierRefExpr . head $ memberRefs else MemberRefExpr memberRefs) <$> memberRefs where
        memberRefs = pure <$> parseIdentifier input ->> parseMemberRefCont
        parseMemberRefCont (v, input) = case input of
            LocatedString ('.':xs) _ -> into (next input) ->> dropSpaces ->> (\(_, r) -> (\x -> v ++ [x]) <$> parseIdentifier r) ->> parseMemberRefCont
            _ -> Success (v, input)
    _ -> Failed input
parseFunctionCandidates input = Failed input
