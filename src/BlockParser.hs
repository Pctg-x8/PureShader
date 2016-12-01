module BlockParser(FormulaBlock(..), indentLevel, takeFormula) where

import Common
import PSParser (ParseResult(Success, Failed))

-- Block Parser(Source code into Formula Block)

data FormulaBlock = FormulaBlock LocatedString [FormulaBlock] [FormulaBlock] deriving Eq
instance Show FormulaBlock where
  show (FormulaBlock ls wh li) = "let " ++ show li ++ " in " ++ show ls ++ " where " ++ show wh

takeFormula :: Int -> LocatedString -> ParseResult FormulaBlock
takeFormula indent input@(_ :@: loc) = let (a, rest) = impl indent ("" :@: loc) input in Success (FormulaBlock a [] [], rest) where
  impl il accum (('\n':str) :@: nloc)
    | checkIndentation (il >=) str = (accum, str :@: newLine nloc)
    | otherwise = let nil = indentLevel str in impl nil accum (drop nil str :@: (forwardN nil . newLine) nloc)
  impl il accum rest@(('-':'-':c:_) :@: _) | not (c `charClassOf` Symbol) = impl il accum $ dropUntilLF rest
  impl il (istr :@: iloc) ((c:str) :@: nloc) = impl il ((istr ++ [c]) :@: iloc) $ str :@: forward nloc
  impl _ accum r = (accum, r)

dropUntilLF :: LocatedString -> LocatedString
dropUntilLF (('\n':str) :@: loc) = str :@: newLine loc
dropUntilLF ((_:str) :@: loc) = dropUntilLF $ str :@: forward loc
dropUntilLF input@("" :@: _) = input

checkIndentation :: (Int -> Bool) -> String -> Bool
checkIndentation comparer input = comparer $ indentLevel input

indentLevel :: String -> Int
indentLevel = impl 0 where
  impl l (' ':t) = let l' = l + 1 in l' `seq` impl l' t
  impl l _ = l
