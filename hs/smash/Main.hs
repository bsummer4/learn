module Main (main) where

import Hiccup

import ClassyPrelude              (ByteString, HashMap)
import Control.Monad              (unless)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.State.Strict (StateT)
import Data.List.NonEmpty         (NonEmpty)

import qualified Data.ByteString.Char8 as B
import qualified Language.Bash.Parse   as Bash
import qualified Language.Bash.Syntax  as Bash
import qualified System.Console.Repl   as LineNoise
import qualified System.Environment    as Sys
import qualified System.IO             as Sys

--------------------------------------------------------------------------------------------------------------

data SmashObj = SmashStr ByteString
              | SmashList [SmashObj]
              | SmashInt Int
              | SmashFloat64 Double

data InChan
data OutChan

data SmashFrame = MkSmashFrame
  { input  :: InChan
  , output :: OutChan
  , vars   :: HashMap ByteString SmashObj
  }

newtype SmashCtx = MkSmashCtx (NonEmpty SmashFrame)

type SmashIO = StateT SmashCtx IO

--------------------------------------------------------------------------------------------------------------

type Repl = LineNoise.ReplT IO

main :: IO ()
main = do
  Sys.hSetBuffering Sys.stdout Sys.NoBuffering
  Sys.getArgs >>= \case
    []     -> mkInterp >>= runRepl
    (f:fs) -> do fdata <- B.readFile f
                 runTclWithArgs fdata (map B.pack fs) >>= (`unlessErr` (\_ -> return ()))

unlessErr :: Either B.ByteString b -> (b -> IO ()) -> IO ()
unlessErr x f = either (\e -> B.putStrLn e) f x

runRepl :: Hiccup.Interpreter -> IO ()
runRepl i = LineNoise.runRepl (repl i) LineNoise.defaultSettings

repl :: Hiccup.Interpreter -> Repl ()
repl i = loop
 where
  loop :: Repl ()
  loop = LineNoise.replM "hiccup> " action completions

  completions :: String -> Repl [String]
  completions _ = pure []

  action :: String -> Repl ()
  action = \case
    "" -> loop
    ln -> do -- addHistory ln
             v <- liftIO $ runInterp (B.pack ln) i
             liftIO $ v `unlessErr` (\o -> unless (B.null o) (B.putStrLn o))
             loop
