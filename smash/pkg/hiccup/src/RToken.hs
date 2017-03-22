module RToken (Cmd, RToken(..), noInterp, singleTok, tryParsed, Parseable, Parsed, asParsed) where

import qualified Data.ByteString.Char8 as B
import BSParse (TclWord(..), doInterp, runParse)
import Util (BString,pack)
import VarName

--------------------------------------------------------------------------------------------------------------

type Parsed = [Cmd]
type Cmd = (Either (NSQual BString) RToken, [RToken])
data RToken = Lit !BString | LitInt !Int | CatLst [RToken]
              | CmdTok Cmd | ExpTok RToken
              | VarRef (NSQual VarName) | ArrRef (Maybe NSTag) !BString RToken
              | Block !BString (Either String [Cmd]) deriving (Eq,Show)

isEmpty (Lit x)    = B.null x
isEmpty (CatLst l) = null l
isEmpty _          = False

noInterp tok = case tok of
   (CmdTok _) -> False
   (VarRef _) -> False
   (ArrRef _ _ _) -> False
   (ExpTok t) -> noInterp t
   (CatLst l) -> all noInterp l
   _          -> True


-- Bit hacky, but better than no literal handling
litIfy s
 | B.length s == 1 = let c = B.index s 0
                     in case c of
                          '0' -> LitInt 0
                          '1' -> LitInt 1
                          '2' -> LitInt 2
                          _   -> Lit s
 | otherwise       = Lit s


compile :: BString -> RToken
compile str = case doInterp str of
                   Left s  -> litIfy s
                   Right x -> handle x
 where f (Left match) = case parseVarName match of
                          NSQual ns (VarName n (Just ind)) -> ArrRef ns n (compile ind)
                          vn                               -> VarRef vn
       f (Right x)    = compCmd x
       handle (b,m,a) = let front = [Lit b, f m]
                        in let lst = filter (not . isEmpty) (front ++ [compile a])
                           in case lst of
                                [a] -> a
                                _   -> CatLst lst

compToken :: TclWord -> RToken
compToken (Word s)               = compile s
compToken (NoSub s res)          = Block s (fromParsed res)
compToken (Expand t)             = ExpTok (compToken t)
compToken (Subcommand c)         = compCmd c

compCmd c = CmdTok (toCmd c)

class Parseable a where
  asParsed :: (Monad m) => a -> m Parsed

instance Parseable B.ByteString where
  asParsed s = case tryParsed s of
                  Left s -> fail s
                  Right p -> return p

tryParsed :: BString -> Either String Parsed
tryParsed s = case runParse s of
                Nothing -> Left $ "parse failed: " ++ show s
                Just (r,rs) -> if B.null rs then Right (map toCmd r) else Left ("Incomplete parse: " ++ show rs)

fromParsed Nothing       = Left "parse failed"
fromParsed (Just (tl,v)) = if B.null v then Right (map toCmd tl) else Left ("incomplete parse: " ++ show v)


toCmd (x,xs) = (handleProc (compToken x), map compToken xs)
  where handleProc (Lit v) = Left (parseProc v)
        handleProc xx      = Right xx

singleTok b = [toCmd (Word b,[])]
