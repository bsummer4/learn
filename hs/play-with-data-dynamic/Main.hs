module Main where

{-
Alright, I want to play with the idea of interacting with Haskell objects
from an unit-typed language like TCL or Lua. Both of these languages interact
with foreign data structures by keeping a table of of foreign references
and using keys to that table as references to these forign objects[1]

[1]: Is this actually true?
-}

import ClassyPrelude
import Data.IORef
import Control.Applicative
import Data.Monoid
import Data.Typeable

import Data.Containers    (insert, lookup)
import Data.Dynamic       (Dynamic, fromDynamic, toDyn)
import Data.IntMap.Strict (IntMap)
import System.IO.Unsafe   (unsafePerformIO)

newtype ForeignTable = MkForeignTable (IntMap Dynamic)
newtype ForeignRef   = MkForeignRef Int

foreignRefs :: IORef (Int, ForeignTable)
foreignRefs = unsafePerformIO (newIORef (0, mempty))

exportForeign :: Typeable a => a -> IO Int
exportForeign newData =
  atomicModifyIORef foreignRefs $ \(nextKey, refs) ->
    let updatedRefs = insert nextKey (toDyn newData)
      in (nextKey, (nextKey+1, updatedRefs))

importForeign :: Typeable a => Int -> IO (Maybe a)
importForeign k = (pure . snd >>= lookup k >>= fromDynamic) <$> readIORef foreignRefs


main :: IO ()
main = do
  k <- exportForeign ("hello world" :: Text)
  Just msg <- importForeign k
  putStrLn msg

{-
Cool.

Now I need to be able to actually operate on this data.

Given a monomorphic operation, I should be able to apply the operation to
a dynamic value and simply error if it has the wrong type.

Unfourtunatly, the Haskell ecosystem uses *very* few monomorphic operations.

We can also pretty easily work with ad-hoc polymorphic data using tables
of methods. I guess the only choice here is to pack up Haskell data as objects?
-}

newtype HsProc = HsProc (Vec Dynamic -> IO Dynamic)
newtype Object = MkObject (Vec Dynamic -> IO Dynamic)
newtype Actor  = MkActor (Vec Dynamic -> IO ())

{-
This works, but Haskell doesn't provide great tools for working with Dynamic values. How
about some Template Haskell?

hsstrlen = [smashproc|
  [v∷String] -> Text.length v
|]

hsvec = [smashproc|
  []           -> Vector.empty
  [x∷Dynamic]  -> Vector.singleton sing
  xs∷[Dynamic] -> Vector.fromList xs
|]

We would need to run the type inference engine on each snippet, make sure
that each argument has a monomorphic type, then generate code to
examine the types (and counts) of the actual arguments, throw errors if
we're given arguments that don't match any cases, unpack all the `Dynamic`
boxes, and then run the right branch with the unpacked values.
-}
