module Main (main) where

import Prelude
import Data.Foldable (for_)

import qualified TryAlex.LexChars as LC

main = do
  s <- getContents
  for_ (LC.alexScanTokens s) $ \case
    LC.TChar c -> putChar c
