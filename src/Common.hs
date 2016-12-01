{-# LANGUAGE TypeOperators #-}

module Common(Location(..), LocatedString(..), Concatable(..), append, initLocation, forward, forwardN, newLine,
  PairDirection(..), CharacterClass(..), characterClass, charClassOf, isPartOfIdentifier) where

-- Common Structures

-- Location
data Location = Location Int Int deriving Eq
instance Show Location where show (Location l c) = show l ++ ":" ++ show c
initLocation :: Location
initLocation = Location 1 1
forward :: Location -> Location
forward (Location l c) = Location l (c + 1)
forwardN :: Int -> Location -> Location
forwardN n (Location l c) = Location l (c + n)
newLine :: Location -> Location
newLine (Location l _) = Location (l + 1) 1

-- String with Location
data LocatedString = String :@: Location deriving Eq
instance Show LocatedString where show (s :@: l) = s ++ " at " ++ show l

-- Concatenate Operations
class Concatable a where
  (~~) :: a -> a -> a
instance Concatable LocatedString where
  (sa :@: l) ~~ (sb :@: _) = (sa ++ sb) :@: l
append :: LocatedString -> String -> LocatedString
append (sa :@: l) sb = (sa ++ sb) :@: l

-- Character Classifications --
data PairDirection = Open | Close deriving Eq
data CharacterClass = Ignore | InternalSymbol | LineFeed | Parenthese PairDirection | Bracket PairDirection | Symbol | Number | Other deriving Eq
characterClass :: Char -> CharacterClass
characterClass c
    | c `elem` " \t" = Ignore
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
