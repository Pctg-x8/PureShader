module BlockParser(FormulaBlock(..), indentLevel, takeFormula) where

import Common

-- Block Parser(Source code into Formula Block)

data FormulaBlock = Formula LocatedString deriving Eq
instance Show FormulaBlock where
  show (Formula ls) = show ls

takeFormula :: Int -> LocatedString -> FormulaBlock
takeFormula indent input@(_ :@: loc) = Formula $ impl ("" :@: loc) input where
  impl accum (('\n':str) :@: _) | checkIndentation (indent <=) str = accum
  impl accum rest@(('-':'-':c:_) :@: _) | not (c `charClassOf` Symbol) = impl accum $ dropUntilLF rest
  impl (istr :@: iloc) ((c:str) :@: nloc) = impl ((istr ++ [c]) :@: iloc) $ str :@: forward nloc
  impl accum _ = accum

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
