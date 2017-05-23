module Main (main) where

import Prelude
import Data.Foldable (for_)

import qualified TryAlex.LexLines as LL

main = do
  s <- getContents
  for_ (LL.alexScanTokens s) $ \case
    LL.TLine s -> putStr s
