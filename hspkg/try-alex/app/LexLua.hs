{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import ClassyPrelude

import qualified Language.Lua.Annotated.Lexer as LL

main :: IO ()
main = do
  s <- hGetContents stdin
  traverse_ print (LL.llex s)
