{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module VarName (parseVarName,
                nsTail,
                nsQualifiers,
                parseNSTag,
                parseProc,
                VarName(..),
                arrName,
                isArr,
                showVN,
                NSQual(..),
                NSTag(..),
                asGlobal,
                isGlobalQual,
                noNsQual,
                splitWith,
                nsSep) where

import Util
import qualified Data.ByteString.Char8 as B
import Data.ByteString (findSubstrings)

--------------------------------------------------------------------------------------------------------------

data NSQual a = NSQual !(Maybe NSTag) !a deriving (Eq,Show)

data NSTag = NS !Bool ![BString] deriving (Eq,Show,Ord)

data VarName = VarName { vnName :: !BString, vnInd :: Maybe BString } deriving (Eq,Show)

class BStringable a where
  toBStr :: a -> BString

instance BStringable NSTag where
  toBStr (NS g lst) = B.append (if g then nsSep else B.empty) (B.intercalate nsSep lst)

arrName !an !ind = VarName an (Just ind)
{-# INLINE arrName #-}

isArr (VarName _ (Just _)) = True
isArr _                    = False

nsSep :: BString
nsSep = "::"

isGlobalQual (Just (NS v _)) = v
isGlobalQual _               = False
{-# INLINE isGlobalQual #-}

noNsQual nst = case nst of
         Nothing              -> True
         (Just (NS False [])) -> True
         _                    -> False
{-# INLINE noNsQual #-}

asGlobal (Just (NS _ lst)) = Just (NS True lst)
asGlobal Nothing           = Just (NS True [])

parseNSQual ns = case parseNSTag ns of
                  Nothing -> NSQual Nothing ""
                  Just (NS False [s]) -> NSQual Nothing s
                  Just (NS gq []) -> NSQual (Just (NS gq [])) ""
                  Just (NS gq nsl) -> NSQual (Just (NS gq (init nsl))) (last nsl)


parseNSTag ns = toNSTag (ns `splitWith` nsSep) where
   toNSTag nl = if isAbs then return (NS True (tail nl))
                         else return (NS False nl)
   isAbs = nsSep == B.take 2 ns

parseVarName name =
   case parseArrRef name of
     (str,ind) -> case parseNSQual str of
                    NSQual nst n -> NSQual nst (VarName n ind)

parseProc :: BString -> NSQual BString
parseProc = parseNSQual

showVN :: VarName -> String
showVN (VarName name Nothing) = show name
showVN (VarName name (Just i)) = "\"" ++ unpack name ++ "(" ++ unpack i ++ ")\""

parseArrRef str = case B.elemIndex '(' str of
             Nothing    -> (str, Nothing)
             Just start -> if (start /= 0) && B.last str == ')'
                             then let (pre,post) = B.splitAt start str
                                  in (pre, Just (B.tail (B.init post)))
                             else (str, Nothing)
nsTail (NS _ []) = error "Malformed NSTag"
nsTail (NS _ nsl) = last nsl

nsTail_ x = case parseNSTag x of
              Nothing -> error "FAIL"
              Just v -> nsTail v


nsQualifiers str = case findSubstrings nsSep str of
                      [] -> B.empty
                      lst -> B.take (last lst) str


splitWith :: BString -> BString -> [BString]
splitWith str sep =
    case findSubstrings sep str of
        []     -> [str]
        il     -> extract il str
 where slen              = B.length sep
       extract [] !s     = [s]
       extract (i:ix) !s = let (b,a) = B.splitAt i s
                          in b : extract (map (\v -> v - (i+slen)) ix) (B.drop slen a)
{-# INLINE splitWith #-}
