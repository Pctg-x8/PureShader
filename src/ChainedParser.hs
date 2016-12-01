module ChainedParser(ParseResult(Success, Failed), IntoParserChain(into), ParserChain(..)) where

import Common(LocatedString)
-- Chained Parser

-- The result of Parser
data ParseResult a = Success (a, LocatedString) | Failed LocatedString deriving Eq
instance Show a => Show (ParseResult a) where
  show (Success (v, r)) = "Success " ++ show v ++ " remains " ++ show r
  show (Failed r) = "Failed remaining " ++ show r
instance Functor ParseResult where
  fmap f (Success (v, r)) = Success (f v, r)
  fmap _ (Failed r) = Failed r
-- Lifts up into parser chain as remainings
class IntoParserChain a where
  into :: a -> ParseResult ()
instance IntoParserChain LocatedString where into l = Success ((), l)

class ParserChain c where
  (/>) :: c a -> ((a, LocatedString) -> c b) -> c b
  (//) :: c a -> (LocatedString -> c a) -> c a
  (|=>) :: ((x, LocatedString) -> c a) -> (a -> b) -> (x, LocatedString) -> c b
  infixl 2 />
  infixl 1 //
  infixl 3 |=>

instance ParserChain ParseResult where
  (Success x) /> f = f x
  (Failed r) /> _ = Failed r
  r@(Success _) // _ = r
  (Failed r) // f = f r
  (partialFunc |=> reducer) x = reducer <$> partialFunc x
