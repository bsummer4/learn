{
module TryAlex.LexLines (Token(..), alexScanTokens) where
}

%wrapper "basic"

tokens :-
  .+[\n\r] { \l -> TLine l }

{
data Token = TLine [Char]
}
