{-# LANGUAGE BangPatterns #-}
module TclLib.UtilProcs ( utilProcs ) where

import Data.Time.Clock (diffUTCTime,getCurrentTime,addUTCTime)
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Core (evalTcl, subst, callProc)
import Common
import Util (unpack)
import ExprParse
import Data.List (intersperse)
import qualified TclObj as T

utilProcs = makeCmdMap [
   ("time", procTime),
   ("incr", procIncr), ("expr", procExpr),
   ("after", procAfter), ("update", procUpdate)]

procIncr args = case args of
         [vname]     -> incr vname 1
         [vname,val] -> T.asInt val >>= incr vname
         _           -> argErr "incr"

incr :: T.TclObj -> Int -> TclM RetVal
incr n !i =  varModify (T.asBStr n) $
                 \v -> do ival <- T.asInt v
                          return $! (T.mkTclInt (ival + i))

procTime args =
   case args of
     [code]     -> do tspan <- dotime code
                      return (T.mkTclStr (show tspan))
     [code,cnt] -> do count <- T.asInt cnt
                      unless (count > 0) (tclErr "invalid number of iterations in time")
                      ts <- mapM (\_ -> dotime code) [1..count]
                      let str = show ((sum ts) / fromIntegral (length ts))
                      return (T.mkTclStr (str ++ " per iteration"))
     _      -> argErr "time"
 where dotime code = do
         startt <- io getCurrentTime
         evalTcl code
         endt <- io getCurrentTime
         let tspan = diffUTCTime endt startt
         return tspan

procAfter args =
    case args of
      [mss]    -> do
            ms <- T.asInt mss
            io $ threadDelay (1000 * ms)
            ret
      (mss:acts) -> do
            ms <- T.asInt mss
            let secs = (fromIntegral ms) / 1000.0
            currT <- io getCurrentTime
            let dline = addUTCTime secs currT
            evtAdd (T.objconcat acts) dline
      _     -> argErr "after"

procUpdate args = case args of
     [] -> do evts <- evtGetDue
              upglobal (mapM_ evalTcl evts)
              ret
     _  -> argErr "update"
 where upglobal f = do sl <- stackLevel
                       uplevel sl f

procExpr args = do
  al <- mapM subst args
  let s = concat $ intersperse " " (map unpack al)
  riExpr s lu
 where lu v = case v of
                Left n      -> varGet n
                Right (n,a) -> callProc n a
