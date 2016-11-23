module PSParser (Location(Location), LocatedString(LocatedString), initLocation, ParseResult(Success, Failed), parseNumber, parseIdentifier,
    ExpressionNode(..), parseExpression) where

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\t' = True
isSpace '\n' = True
isSpace _ = False

data Location = Location { line :: Int, column :: Int } deriving Eq
instance Show Location where
    show loc = line_str ++ ":" ++ column_str where
        line_str = show $ line loc
        column_str = show $ column loc
data LocatedString = LocatedString String Location deriving Eq
instance Show LocatedString where
    show (LocatedString str loc) = str ++ " at " ++ show loc
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
infixl 1 ->>
(Success x) ->> f = f x
(Failed r) ->> _ = Failed r

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
data CharacterClass = Ignore | InternalSymbol | Parenthese PairDirection | Symbol | Number | Other deriving Eq
determineCharacterClass :: Char -> CharacterClass
determineCharacterClass c
    | isSpace c = Ignore
    | c == '(' = Parenthese Open
    | c == ')' = Parenthese Close
    | any (== c) "+-*/<>$?" = Symbol
    | any (== c) "." = InternalSymbol
    | '0' <= c && c <= '9' = Number
    | otherwise = Other

dropSpaces :: (a, LocatedString) -> ParseResult a
dropSpaces (v, input) = Success $ case input of
    LocatedString str@(c:_) loc | isSpace c -> let nSpaces = countSatisfyElements isSpace str in (v, LocatedString (drop nSpaces str) (forwardN nSpaces loc))
    _ -> (v, input)
ensureCharacter :: Char -> (a, LocatedString) -> ParseResult a
ensureCharacter c (v, input) = case input of
    LocatedString (x:xs) loc | x == c -> Success (v, LocatedString xs $ forward loc)
    _ -> Failed input
ignorePrevious :: (LocatedString -> ParseResult a) -> (b, LocatedString) -> ParseResult a
ignorePrevious f (_, r) = f r

-- Primitive Parsing --
parseNumber :: LocatedString -> ParseResult LocatedString
parseNumber input = case input of
    LocatedString (c:_) _ | '0' <= c && c <= '9' -> Success $ takeStrOpt (\x -> determineCharacterClass x == Number) input
    _ -> Failed input

parseIdentifier :: LocatedString -> ParseResult LocatedString
parseIdentifier input = case input of
    LocatedString (c:_) _ | determineCharacterClass c == Other -> Success $ takeStrOpt (\x -> determineCharacterClass x == Other) input
    _ -> Failed input

-- Expression --
data ExpressionNode = IdentifierRefExpr LocatedString | NumberConstExpr LocatedString | MemberRefExpr [LocatedString] deriving Eq
instance Show ExpressionNode where
    show (IdentifierRefExpr loc) = "IdentifierRefExpr " ++ show loc
    show (NumberConstExpr loc) = "NumberConstExpr " ++ show loc
    show (MemberRefExpr locs) = "MemberRefExpr " ++ show locs

parseExpression :: LocatedString -> ParseResult ExpressionNode
parseExpression = parsePrimaryTerm

parsePrimaryTerm :: LocatedString -> ParseResult ExpressionNode
parsePrimaryTerm input@(LocatedString (c:_) _) = case determineCharacterClass c of
    Number -> NumberConstExpr <$> parseNumber input
    Parenthese Open -> into (next input) ->> dropSpaces ->> ignorePrevious parseExpression ->> dropSpaces ->> ensureCharacter ')'
    Other -> (\memberRefs -> if length memberRefs == 1 then IdentifierRefExpr . head $ memberRefs else MemberRefExpr memberRefs) <$> memberRefs where
        memberRefs = pure <$> parseIdentifier input ->> dropSpaces ->> parseMemberRefCont
        parseMemberRefCont (v, input) = case input of
            LocatedString ('.':xs) _ -> into (next input) ->> dropSpaces ->> (\(_, r) -> (\x -> v ++ [x]) <$> parseIdentifier r) ->> dropSpaces ->> parseMemberRefCont
            _ -> Success (v, input)
    _ -> Failed input
parsePrimaryTerm input = Failed input
