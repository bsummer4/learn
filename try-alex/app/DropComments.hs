module Main (main) where

import ClassyPrelude

import qualified TryAlex.LexComments as LC

(&) = flip ($)

main = do
  s <- getContents
  LC.alexScanTokens s & \case
    Left err     -> error err
    Right tokens -> for_ tokens $ \case
      LC.TChar s -> putChar s
      LC.TEof    -> pure ()
