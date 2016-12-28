{
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module TryAlex.LexComments (Token(..), alexScanTokens) where
}

%wrapper "monad"

$anychar   = [ . \n ]

@comment_start = "/*"
@comment_end   = "*/"

tokens :-
  <0> @comment_start { begin comment }

  <0> $anychar {
    \(_, _, _, is) len ->
      case (take len is) of
        [c] -> pure (TChar c)
        cs  -> error $ "This should never match more than one character! '" ++ cs ++ "'"
  }

  <comment> @comment_end { begin 0 }
  <comment> .            ;

{
data Token
  = TChar Char
  | TEof

alexEOF = pure TEof

alexScanTokens :: String -> Either String [Token]
alexScanTokens str = runAlex str $ loop []
  where
    loop acc =
      alexMonadScan >>= \case
        TEof -> return $ reverse (TEof : acc)
        tok  -> loop $! (tok : acc)
}
