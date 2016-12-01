module BlockParser(FormulaBlock(..), indentLevel, takeFormula, takeClauseFormulaList) where

import Common
import ChainedParser

-- Block Parser(Source code into Formula Block)

data FormulaBlock = FormulaBlock LocatedString [FormulaBlock] [FormulaBlock] deriving Eq
instance Show FormulaBlock where
  show (FormulaBlock ls wh li) = "let " ++ show li ++ " in " ++ show ls ++ " where " ++ show wh

withIndent :: LocatedString -> (Int, LocatedString)
withIndent (str :@: loc) = let l = indentLevel str in (l, drop l str :@: forwardN l loc)
unspaced :: LocatedString -> LocatedString
unspaced ((' ':r) :@: l) = unspaced (r :@: forward l)
unspaced v = v

takeFormula :: Int -> LocatedString -> ParseResult FormulaBlock
takeFormula indent input@(_ :@: loc) = impl indent ("" :@: loc) input where
  impl il accum (('w':'h':'e':'r':'e':rest@(c:_)) :@: nloc) | characterClass c `notElem` [Other, Number] =
    (\x -> FormulaBlock accum x []) <$> takeWhereClause il (unspaced (rest :@: forwardN 5 nloc))
  impl il accum (('\n':str) :@: nloc) = let (indent', line) = withIndent (str :@: newLine nloc) in
    if indent' > il then impl indent' accum line else case line of
      (('w':'h':'e':'r':'e':c:_) :@: _) | not $ isPartOfIdentifier c -> impl indent' accum line
      _ -> Success (FormulaBlock accum [] [], str :@: newLine nloc)
  impl il accum rest@(('-':'-':c:_) :@: _) | not (c `charClassOf` Symbol) = impl il accum $ dropUntilLF rest
  impl il (istr :@: iloc) ((c:str) :@: nloc) = impl il ((istr ++ [c]) :@: iloc) $ str :@: forward nloc
  impl _ accum r = Success (FormulaBlock accum [] [], r)
takeClauseFormulaList :: Int -> LocatedString -> ParseResult [FormulaBlock]
takeClauseFormulaList firstIndent input = (: []) <$> takeFormula firstIndent input /> takeTails where
  takeTails (x, str :@: rloc)
    | checkIndentation (firstIndent ==) str = let nil = indentLevel str; r' = drop nil str :@: forwardN nil rloc in (\e -> x ++ [e]) <$> takeFormula firstIndent r' /> takeTails
  takeTails v = Success v

takeWhereClause :: Int -> LocatedString -> ParseResult [FormulaBlock]
takeWhereClause currentIndent (('\n':r) :@: l) = let (il, lr) = withIndent (r :@: newLine l) in
  if il > currentIndent then takeClauseFormulaList il lr else Success ([], r :@: newLine l)
takeWhereClause _ r@((_:_) :@: Location _ c) = takeClauseFormulaList (c - 1) r
takeWhereClause _ r@("" :@: _) = Success ([], r)

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
