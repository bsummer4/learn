{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SideMain where

import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Prelude                        hiding (head)

import System.IO.Error as IO (catchIOError, ioError, isEOFError)
import Control.Monad.IO.Class (liftIO)

--------------------------------------------------------------------------------------------------------------

-- import Data.Function          ((&))

infixl 1 &

(&) :: a -> (a -> b) -> b
v & f = f v

--------------------------------------------------------------------------------------------------------------

data TChan a
data STM a
data Async a

async :: IO a -> IO (Async a)
async = undefined

wait :: Async a -> IO a
wait = undefined

waitBoth :: Async a -> Async b -> IO (a, b)
waitBoth = undefined

atomically :: STM a -> IO a
atomically = undefined

sendChan :: TChan a -> a -> STM ()
sendChan = undefined

recvChan :: TChan a -> STM (Maybe a)
recvChan = undefined

forkIO :: IO a -> IO Int
forkIO = undefined

newTChanIO :: IO (TChan a)
newTChanIO = undefined


--------------------------------------------------------------------------------------------------------------

newtype Map k v    = MkMap [(k, v)]
newtype NonEmpty a = MkNonEmpty (a, [a])

newtype Str = MkStr String

data Val = StrVal Str
         | NilVal

instance Show Val where
  show (StrVal (MkStr s)) = s
  show NilVal             = ""

newtype InChan  = MkInChan (IO (Maybe Val))
newtype OutChan = MkOutChan (Val -> IO ())

data Frame = MkFrame { input  :: InChan
                     , output :: OutChan
                     , env    :: Map Str Val
                     }

newtype Stack   = MkStack { unStack :: NonEmpty Frame }
newtype SmashM a = MkSmash { runSmashM :: StateT Stack IO a }
  deriving (Functor, Applicative, Monad)

type Smash = SmashM Val

pass :: SmashM ()
pass = MkSmash $ return ()

head :: NonEmpty a -> a
head (MkNonEmpty (x, _)) = x

yield :: Val -> SmashM ()
yield v = MkSmash $ do
    MkStack (MkNonEmpty (topFrame, _)) <- get
    topFrame & \case
        MkFrame { output=(MkOutChan send), .. } -> do
            liftIO $ send v

recv :: SmashM (Maybe Val)
recv = MkSmash $ do
    MkStack (MkNonEmpty (topFrame, _)) <- get
    topFrame & \case
        MkFrame { input=(MkInChan recvVal), .. } -> do
            v <- liftIO recvVal
            return v

withOutput :: OutChan -> SmashM a -> SmashM a
withOutput c (MkSmash a) =
    MkSmash $ withStateT f a
  where
    f st = case st of
      MkStack (MkNonEmpty (topFrame, otherFrames)) ->
        let updatedTopFrame = topFrame { output = undefined }
          in MkStack $ MkNonEmpty (updatedTopFrame, otherFrames)

withInput :: InChan -> SmashM a -> SmashM a
withInput c (MkSmash a) =
    MkSmash $ withStateT f a
  where
    f st = case st of
      MkStack (MkNonEmpty (topFrame, otherFrames)) ->
        let updatedTopFrame = topFrame { input = undefined }
          in MkStack $ MkNonEmpty (updatedTopFrame, otherFrames)

infixl 5 .|.
(.|.) :: SmashM () -> SmashM b -> SmashM b
(.|.) sender receiver = MkSmash $ do
  pipe <- liftIO newTChanIO
  st   <- get

  let writeToPipe  = MkOutChan $ \(x :: Val) -> atomically $ sendChan pipe x
      readFromPipe = MkInChan $ atomically $ recvChan pipe

  liftIO $ do
    sendT <- async $ evalStateT (runSmashM $ withOutput writeToPipe sender) st
    recvT <- async $ evalStateT (runSmashM $ withInput readFromPipe receiver) st
    snd <$> waitBoth sendT recvT

smash :: SmashM ()
smash = do
  Just v <- yield (StrVal (MkStr "asdf")) .|. recv
  yield v

stdin :: InChan
stdin = MkInChan $ do
    catchIOError (Just <$> StrVal <$> MkStr <$> getLine) handleErr
  where
    handleErr :: IOError -> IO (Maybe a)
    handleErr e = if IO.isEOFError e
                  then return Nothing
                  else ioError e

stdout :: OutChan
stdout = MkOutChan $ \v ->
    print v

topFrame :: Frame
topFrame = MkFrame { input = stdin
                   , output = stdout
                   , env = MkMap []
                   }

initialState :: Stack
initialState = MkStack $ MkNonEmpty (topFrame, [])

main :: IO ()
main = evalStateT (runSmashM smash) initialState
