module ProcArgs (parseParams, bindArgs, ParamList) where

import Util
import qualified TclObj as T
import Common
import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------------------------------------

type ArgSpec = Either BString (BString,T.TclObj)
type ArgList = [ArgSpec]

showParams (n,hasArgs,pl) =
   show ((n:(map arg2name pl)) `joinWith` ' ') ++ if hasArgs then " ..." else ""

arg2name arg = case arg of
               Left s      -> s
               Right (k,_) -> B.cons '?' (B.snoc k '?')

type ParamList = (BString, Bool, ArgList)

mkParamList :: BString -> ArgList -> ParamList
mkParamList name lst = (name, hasArgs, used)
 where hasArgs = (not . null) lst && (lastIsArgs lst)
       used = if hasArgs then init lst else lst
       lastIsArgs = either (== (pack "args")) (const False)  . last


parseParams :: BString -> T.TclObj -> TclM ParamList
parseParams name args = T.asList args >>= countRet
 where countRet :: [T.TclObj] -> TclM ParamList
       countRet lst = mapM doArg lst >>= return . mkParamList name
       doArg :: T.TclObj -> TclM ArgSpec
       doArg s = do l <- T.asList s
                    return $ case l of
                            [k,v] -> Right (T.asBStr k,v)
                            _     -> Left (T.asBStr s)

bindArgs :: ParamList -> [T.TclObj] -> TclM [(BString,T.TclObj)]
bindArgs params@(_,hasArgs,pl) args = walkBoth pl args []
  where walkBoth ((Left v):xs)      (a:as) !acc = walkBoth xs as ((v,a):acc)
        walkBoth ((Left _):_)       []      _   = badArgs
        walkBoth ((Right (k,_)):xs) (a:as) !acc = walkBoth xs as ((k,a):acc)
        walkBoth ((Right (k,v)):xs) []     !acc = walkBoth xs [] ((k,v):acc)
        walkBoth []                 xl     !acc = if hasArgs then return $! ((pack "args"),(T.mkTclList xl)):acc
                                                             else if null xl then return $! acc
                                                                             else badArgs
        badArgs = argErr $ "should be " ++ showParams params
