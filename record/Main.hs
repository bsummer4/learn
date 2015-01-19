{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}

module Main where

import Record
import Control.Lens

zero :: Int
zero = 0

type Person =
  [r| {name :: String,
       birthday :: {year :: Int, month :: Int, day :: Int}}
  |]

personExample :: Person
personExample = [r|{name="Hello World!", birthday={year=zero, month=zero, day=zero}}|]

main = putStrLn $ personExample ^. [l|name|]
