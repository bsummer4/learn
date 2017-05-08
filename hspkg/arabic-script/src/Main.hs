{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char     (toLower)
import Data.Foldable (for_)

import qualified Data.Text          as T
import qualified System.Environment as Sys

-- https://alaafiawrites.wordpress.com/2016/10/08/the-english-language-written-in-arabic-script/
convert :: Char -> String
convert = \case
  'a'   -> "ا"  --     apple    ابل
  'b'   -> "ب"  --    ball    بالل
  'c'   -> "ك"  --    camera    كاميرا
--'ch'  -> "ثش" --    chalk    تشالك
  'd'   -> "د"  -- (interchangeable)    drum    دروم/ ضروم
  'e'   -> "اِ"  --    elephant    اِلاِفانت
  'f'   -> "ف"  --    frog    فروغ
  'g'   -> "غ"  --    ghost    غاُوست
  'h'   -> "ه"  -- /ح    hall    هالل/ حالل
  'i'   -> "اِ"  -- / ي--    igloo    اِغلوو يغلوو
  'j'   -> "ج"  -- just    جوست
  'k'   -> "ك"  -- kebab    كاباب
  'l'   -> "ل"  -- lamb    لام
  'm'   -> "م"  -- money    مونيي
  'n'   -> "ن"  -- night    نيت
  'o'   -> "اُ"  -- oak    اُوك
  'p'   -> "ب"  -- pram    برام
  'q'   -> "ق"  -- quack    قواك
  'r'   -> "ر"  -- restaurant    راستورانت
  's'   -> "س"  -- sing    سينغ
  't'   -> "ت"  -- tree    تريي
  'u'   -> "اُ"  -- under    اُندر
  'v'   -> "ف"  -- vibe    فايب
  'w'   -> "و"  -- water    واتر
  'x'   -> "ز"  -- (beginning of a word        xylophone    زايلوفون
--'x'   -> "ك"  -- (middle or end  of a word)        oxidise    اوكسدايس
  'y'   -> "يو" --    young    يونغ
  'z'   -> "ز"  --    zebra    زبرا
  other -> [other]

main :: IO ()
main = do
  args <- Sys.getArgs
  for_ args $ \arg -> do
    putStrLn $ concat $ (convert . toLower) <$> arg
