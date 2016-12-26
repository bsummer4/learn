{
module TryAlex.LexChars (Token(..), alexScanTokens) where
}

%wrapper "basic"

tokens :-
  [ . $white ] {
    \case
      [c] -> TChar c
      _   -> error "This should never match more than one character!"
  }

{
data Token = TChar Char
}
