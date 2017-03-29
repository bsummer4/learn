module Hiccup (runTcl, runTclWithArgs, mkInterp, runInterp, Interpreter) where

import Control.Monad.Error
import Data.IORef

import Util
import qualified TclObj as T
import Core (evalTcl)
import Common
import ProcArgs
import Data.Void (Void)

import TclLib (libCmds)

--------------------------------------------------------------------------------------------------------------

coreProcs = makeCmdMap $
 [("proc", procProc),("set", procSet),("upvar", procUpVar),
  ("rename", procRename),("uplevel", procUpLevel), ("unset", procUnset),("eval", procEval),
  ("return", procReturn),("break", procRetv EBreak),("catch", procCatch),
  ("continue", procRetv EContinue),("error", procError), ("info", procInfo), ("global", procGlobal)]


baseProcs = mergeCmdMaps [libCmds, coreProcs]

processArgs al = [("argc" * T.mkTclInt (length al)), ("argv" * T.mkTclList al)]
  where (*) name val = (pack name, val)

interpVars = [("tcl_version" * (show hiccupVersion))]
  where (*) name val = (pack name, T.mkTclStr val)

hiccupVersion = 0.4

data Interpreter = Interpreter (IORef TclState)

mkInterp = mkInterpWithArgs []

mkInterpWithArgs :: [BString] -> IO Interpreter
mkInterpWithArgs args = do
              st <- makeState (interpVars ++ (processArgs (map T.mkTclBStr args))) baseProcs
              stref <- newIORef st
              return (Interpreter stref)


runInterp :: BString -> Interpreter -> IO (Either BString BString)
runInterp s = runInterp' (evalTcl (T.mkTclBStr s))

runInterp' t (Interpreter i) = do
                 bEnv <- readIORef i
                 (r,i') <- runTclM t bEnv
                 writeIORef i i'
                 return (fixErr r)
  where perr (EDie s)    = Left $ pack s
        perr (ERet v)    = Right $ T.asBStr v
        perr EBreak      = Left . pack $ "invoked \"break\" outside of a loop"
        perr EContinue   = Left . pack $ "invoked \"continue\" outside of a loop"
        fixErr (Left x)  = perr x
        fixErr (Right v) = Right (T.asBStr v)

runTcl v = mkInterp >>= runInterp v
runTclWithArgs v args = mkInterpWithArgs args >>= runInterp v


procSet args = case args of
     [s1,s2] -> varSet (T.asBStr s1) s2
     [s1]    -> varGet (T.asBStr s1)
     _       -> argErr "set"

procUnset args = case args of
     [n]     -> varUnset (T.asBStr n)
     _       -> argErr "unset"

procRename args = case args of
    [old,new] -> renameProc (T.asBStr old) (T.asBStr new) >> ret
    _         -> argErr "rename"

procEval args = case args of
                 []   -> argErr "eval"
                 [s]  -> evalTcl s
                 _    -> evalTcl (T.objconcat args)

procCatch args = case args of
           [s]        -> (evalTcl s >> return T.tclFalse) `catchError` (retInt . retCodeToInt)
           [s,result] -> (evalTcl s >>= varSet (T.asBStr result) >> return T.tclFalse) `catchError` (retReason result)
           _   -> argErr "catch"
 where retCodeToInt c = case c of
                           (EDie _)  -> 1
                           (ERet _)  -> 2
                           EBreak    -> 3
                           EContinue -> 4
       retReason v e = case e of
                         EDie s -> varSet (T.asBStr v) (T.mkTclStr s) >> return T.tclTrue
                         _      -> retInt . retCodeToInt $ e
       retInt = return . T.mkTclInt

procInfo = makeEnsemble "info" [
  matchp "locals" localVars,
  matchp "globals" globalVars,
  matchp "vars" currentVars,
  matchp "commands" commandNames,
  noarg "level"    (liftM T.mkTclInt stackLevel),
  ("exists", info_exists),
  ("body", info_body)]
 where noarg n f = (n, no_args n f)
       matchp n f = (n, matchList ("info " ++ n) f)
       no_args n f args = case args of
                           [] -> f
                           _  -> argErr $ "info " ++ n

matchList name f args = case args of
     []    -> f >>= asTclList
     [pat] -> getMatches pat
     _     -> argErr name
 where getMatches pat = f >>= asTclList . globMatches (T.asBStr pat)

info_exists args = case args of
        [n] -> varExists (T.asBStr n) >>= return . T.fromBool
        _   -> argErr "info exists"

info_body args = case args of
       [n] -> do p <- getProc (T.asBStr n)
                 case p of
                   Nothing -> tclErr $ show (T.asBStr n) ++ " isn't a procedure"
                   Just p  -> treturn (cmdBody p)
       _   -> argErr "info body"

asTclList = return . T.mkTclList . map T.mkTclBStr

procReturn args = case args of
      [s] -> throwError (ERet s)
      []  -> throwError (ERet T.empty)
      _   -> argErr "return"

procRetv c args = case args of
    [] -> throwError c
    _  -> argErr $ st c
 where st EContinue = "continue"
       st EBreak    = "break"
       st _         = "??"

procError [s] = tclErr (T.asStr s)
procError _   = argErr "error"

procUpLevel args = case args of
              [p]    -> uplevel 1 (evalTcl p)
              (si:p) -> T.asInt si >>= \i -> uplevel i (procEval p)
              _      -> argErr "uplevel"

procUpVar :: TclCmd
procUpVar args = case args of
     [d,s]    -> doUp 1 d s
     [si,d,s] -> T.asInt si >>= \i -> doUp i d s
     _        -> argErr "upvar"
 where doUp i d s = upvar i (T.asBStr d) (T.asBStr s) >> ret

procGlobal args = case args of
      [] -> argErr "global"
      _  -> mapM_ (inner . T.asBStr) args >> ret
 where inner g = do len <- stackLevel
                    upvar len g g

procProc args = case args of
  [name,alst,body] -> do
    let pname = T.asBStr name
    params <- parseParams pname alst
    regProc pname (T.asBStr body) (procRunner params body)
    ret
  _               -> argErr "proc"

procRunner :: ProcArgs.ParamList -> T.TclObj -> [T.TclObj] -> TclM T.TclObj
procRunner pl body args = do
  locals <- bindArgs pl args
  withLocalScope locals (evalTcl body `catchError` herr)
 where
  herr :: Err -> TclM T.TclObj
  herr (ERet s)  = return $! s
  herr EBreak    = tclErr "invoked \"break\" outside of a loop"
  herr EContinue = tclErr "invoked \"continue\" outside of a loop"
  herr e         = throwError e
