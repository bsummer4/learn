{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module TclLib.NSProcs (nsProcs) where

import Common
import Core (evalTcl)
import VarName
import qualified TclObj as T

nsProcs = makeCmdMap [("namespace", procNamespace), ("variable", procVariable)]

procNamespace :: [T.TclObj] -> TclM RetVal
procNamespace = makeEnsemble "namespace" [
     ("current", ns_current),
     ("eval", ns_eval),
     ("parent", ns_parent),
     ("children", ns_children),
     ("delete", ns_delete),
     ("tail", ns_tail),
     ("export", ns_export),
     ("import", ns_import),
     ("origin", ns_origin),
     ("qualifiers", ns_qualifiers),
     ("exists", ns_exists)]

procVariable args = case args of
       [n]   -> variableNS (T.asBStr n) Nothing  >> ret
       [n,v] -> variableNS (T.asBStr n) (Just v) >> ret
       _     -> argErr "variable"

ns_current args = case args of
       [] -> currentNS >>= treturn
       _  -> argErr "namespace current"

ns_eval args = case args of
          [nsname, code] -> withNS (T.asBStr nsname) (evalTcl code)
          _              -> argErr "namespace eval"

ns_parent args = case args of
          [] -> parentNS >>= treturn
          _  -> argErr "namespace parent"

ns_export :: TclCmd
ns_export args = case map T.asBStr args of
       []     -> getExportsNS >>= return . T.mkTclList . map T.mkTclBStr
       ("-clear":al) -> mapM_ (exportNS True) al >> ret
       al            -> mapM_ (exportNS False) al >> ret

ns_import args = case map T.asBStr args of
      ("-force":rest) -> mapM_ (importNS True) rest >> ret
      al              -> mapM_ (importNS False) al >> ret

ns_origin :: TclCmd
ns_origin args = case args of
     [pn] -> do pr <- getProc (T.asBStr pn)
                case pr of
                  Nothing -> tclErr $ "invalid command name: " ++ show pn
                  Just p  -> getOrigin p >>= treturn
     _    -> argErr "namespace origin"

ns_children args = case args of
          [] -> childrenNS >>= return . T.mkTclList . map T.mkTclBStr
          _  -> argErr "namespace children"

ns_exists args = case args of
          [nsn] -> existsNS (T.asBStr nsn) >>= return . T.fromBool
          _     -> argErr "namespace exists"

ns_delete args = case args of
   []    -> argErr "namespace delete"
   nsl   -> mapM_ (deleteNS . T.asBStr) nsl >> ret

ns_tail args = case args of
   [s] -> case parseNSTag (T.asBStr s) of
           Nothing -> ret
           Just v -> return . T.mkTclBStr $ (nsTail v)
   _   -> argErr "namespace tail"

ns_qualifiers args = case args of
   [s] -> return . T.mkTclBStr $ (nsQualifiers (T.asBStr s))
   _   -> argErr "namespace qualifiers"
