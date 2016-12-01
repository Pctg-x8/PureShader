module BlockParser(FormulaBlock(..), indentLevel, takeFormula, takeClauseFormulaList) where

import Common
import ChainedParser

-- Block Parser(Source code into Formula Block)

data FormulaBlock = FormulaBlock LocatedString [FormulaBlock] [FormulaBlock] deriving Eq
instance Show FormulaBlock where
  show (FormulaBlock ls wh li) = (if not $ null li then "let " ++ show li ++ " in " else "") ++
    show ls ++
    (if not $ null wh then " where " ++ show wh else "")

withIndent :: LocatedString -> (Int, LocatedString)
withIndent (str :@: loc) = let l = indentLevel str in (l, drop l str :@: forwardN l loc)
unspaced :: LocatedString -> LocatedString
unspaced ((' ':r) :@: l) = unspaced (r :@: forward l)
unspaced v = v

notPartOfIdent :: String -> Bool
notPartOfIdent = not . isPartOfIdentifier . head

takeFormula :: Int -> LocatedString -> ParseResult FormulaBlock
takeFormula _ rest@(('i':'n':r) :@: _) | not . isPartOfIdentifier . head $ r = Failed rest
takeFormula indent (('l':'e':'t':rest) :@: loc) | not $ isPartOfIdentifier $ head rest =
  (takeLetClause indent . unspaced) (rest :@: forwardN 3 loc) /> (\(li, r) -> case r of
    ('\n':rs) :@: l -> let (il, lr) = withIndent (rs :@: newLine l) in
      if il > indent then (\(FormulaBlock f wh _) -> FormulaBlock f wh li) <$> takeFormula il lr else Failed lr
    (_:_) :@: Location _ c -> (\(FormulaBlock f wh _) -> FormulaBlock f wh li) <$> takeFormula (c - 1) r
    _ -> Failed r)
takeFormula indent input@(_ :@: loc) = impl indent ("" :@: loc) input where
  impl il accum (('w':'h':'e':'r':'e':rest) :@: nloc) | notPartOfIdent rest =
    (\x -> FormulaBlock accum x []) <$> takeWhereClause il (unspaced (rest :@: forwardN 5 nloc))
  impl _ accum r@(('i':'n':c:_) :@: _) | notPartOfIdent [c] = Success (FormulaBlock accum [] [], r)
  impl il accum (('\n':str) :@: nloc) = let (indent', line) = withIndent (str :@: newLine nloc) in case line of
    ('w':'h':'e':'r':'e':c:_) :@: _ | notPartOfIdent [c] -> impl indent' accum line
    ('i':'n':c:_) :@: _ | notPartOfIdent [c] -> Success (FormulaBlock accum [] [], line)
    _ -> if indent' > il then impl indent' accum line else Success (FormulaBlock accum [] [], str :@: newLine nloc)
  impl il accum rest@(('-':'-':c:_) :@: _) | not (c `charClassOf` Symbol) = impl il accum $ dropUntilLF rest
  impl il (istr :@: iloc) ((c:str) :@: nloc) = impl il ((istr ++ [c]) :@: iloc) (str :@: forward nloc)
  impl _ accum r = Success (FormulaBlock accum [] [], r)
takeClauseFormulaList :: Int -> LocatedString -> ParseResult [FormulaBlock]
takeClauseFormulaList firstIndent input = (: []) <$> takeFormula firstIndent input /> takeTails where
  takeTails (x, rest@(('i':'n':r) :@: _)) | notPartOfIdent r = Success (x, rest)
  takeTails (x, str :@: rloc)
    | checkIndentation (firstIndent ==) str = let nil = indentLevel str; r' = drop nil str :@: forwardN nil rloc in (\e -> x ++ [e]) <$> takeFormula firstIndent r' /> takeTails
  takeTails v = Success v

takeWhereClause :: Int -> LocatedString -> ParseResult [FormulaBlock]
takeWhereClause currentIndent (('\n':r) :@: l) = let (il, lr) = withIndent (r :@: newLine l) in
  if il > currentIndent then takeClauseFormulaList il lr else Success ([], r :@: newLine l)
takeWhereClause _ r@((_:_) :@: Location _ c) = takeClauseFormulaList (c - 1) r
takeWhereClause _ r@("" :@: _) = Success ([], r)
takeLetClause :: Int -> LocatedString -> ParseResult [FormulaBlock]
takeLetClause currentIndent (('\n':r) :@: l) = let (il, lr) = withIndent (r :@: newLine l) in
  if il > currentIndent then takeClauseFormulaList il lr /> letTail else letTail ([], lr)
takeLetClause _ r@((_:_) :@: Location _ c) = takeClauseFormulaList (c - 1) r /> letTail
takeLetClause _ r@("" :@: _) = Failed r
letTail :: ([FormulaBlock], LocatedString) -> ParseResult [FormulaBlock]
letTail (x, ('i':'n':s) :@: l) | notPartOfIdent s = Success (x, unspaced (s :@: forwardN 2 l))
letTail (_, r) = Failed r

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
