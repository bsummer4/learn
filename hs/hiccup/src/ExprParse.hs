module ExprParse (riExpr, exprCompile) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import qualified TclLib.MathProcs as Math

import qualified TclObj as T
import qualified Data.Map as M

import Util

--------------------------------------------------------------------------------------------------------------

tInt :: Int -> TExp
tInt i = TVal (T.mkTclInt i)

tStr :: String -> TExp
tStr s = TVal (T.mkTclStr s)

tFloat :: Double -> TExp
tFloat f = TVal (T.mkTclDouble f)

riExpr s f = expr s >>= \e -> runExpr e f

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef

intOrFloat = P.naturalOrFloat lexer
symbol  = P.symbol lexer
schar c = char c >> P.whiteSpace lexer
identifier  = P.identifier lexer
stringLit = P.stringLiteral lexer

data Op = OpDiv | OpPlus | OpMinus | OpTimes | OpEql | OpNeql |
          OpLt | OpGt | OpLte | OpGte | OpStrNe | OpStrEq | OpAnd |
          OpOr
  deriving (Show,Eq)

data TExp = TOp !Op TExp TExp | TNot TExp | TVar String | TFun String [TExp] | TVal T.TclObj deriving (Show,Eq)

exprCompile :: (Monad m) => String -> m (Callback m -> m T.TclObj)
exprCompile s = do v <- expr s
                   return $ runExpr v


expr s = case parse pexpr "" s of
           Left err -> fail $ "Bad parse: (" ++ s ++ "): " ++ show err
           Right res -> return res

instance Num TExp where
  a + b = TOp OpPlus a b
  (-) = TOp OpMinus
  (*) = TOp OpTimes
  abs = undefined
  signum = undefined
  negate = undefined
  fromInteger i =  TVal (T.mkTclInt (fromIntegral i))

(.&&) = TOp OpAnd
(.||) = TOp OpOr

(.<) = TOp OpLt
(.<=) = TOp OpLte
(.>) = TOp OpGt
(.>=) = TOp OpGte
(.==) = TOp OpEql

eq = TOp OpStrEq
ne = TOp OpStrNe

objapply :: (Monad m) => Callback m -> (T.TclObj -> T.TclObj -> m T.TclObj) -> TExp -> TExp -> m T.TclObj
objapply lu f x y = do
  i1 <- runExpr x lu
  i2 <- runExpr y lu
  f i1 i2
{-# INLINE objapply #-}

funapply lu fn al = do
  args <- mapM (\v -> runExpr v lu) al
  lu (mkCmd fn args)

type CBData = Either BString (BString, [T.TclObj])
type Callback m = (CBData -> m T.TclObj)

mkCmd a b = Right (pack a,b)

runExpr :: (Monad m) => TExp -> Callback m -> m T.TclObj
runExpr exp lu =
  case exp of
    (TOp OpPlus a b) -> objap Math.plus a b
    (TOp OpTimes a b) -> objap Math.times a b
    (TOp OpMinus a b) -> objap Math.minus a b
    (TOp OpDiv a b) -> objap Math.divide a b
    (TOp OpEql a b) -> objap (up Math.equals) a b
    (TOp OpLt a b) -> objap (up Math.lessThan) a b
    (TOp OpNeql a b) -> objap (up Math.notEquals) a b
    (TOp OpGt a b) -> objap (up Math.greaterThan) a b
    (TOp OpLte a b) -> objap (up Math.lessThanEq) a b
    (TOp OpGte a b) -> objap (up Math.greaterThanEq) a b
    (TOp OpStrEq a b) -> objap (sup T.strEq) a b
    (TOp OpStrNe a b) -> objap (sup T.strNe) a b
    (TOp OpAnd a b) -> objap (procBool (&&)) a b
    (TOp OpOr a b) -> objap (procBool (||)) a b
    (TNot v) -> runExpr v lu >>= return . T.fromBool . not . T.asBool
    (TVal v) -> return $! v
    (TVar n) -> lu (Left (pack n))
    (TFun fn al)  -> funapply lu fn al
 where objap = objapply lu
       up f a b = return (f a b)
       sup f a b = return (T.fromBool (f a b))

procBool f a b = do
   let ab = T.asBool a
   let bb = T.asBool b
   return $! T.fromBool (ab `f` bb)

pexpr :: Parser TExp
pexpr   = do
    many space
    res <- myexpr
    many space
    eof
    return res

myexpr = buildExpressionParser table factor

table = [[op1 '*' (OpTimes) AssocLeft, op1 '/' (OpDiv)  AssocLeft]
        ,[op1 '+' (OpPlus) AssocLeft, op1 '-' (OpMinus) AssocLeft]
        ,[op "==" (OpEql) AssocLeft, op "!=" (OpNeql) AssocLeft]
        ,[op "eq" OpStrEq AssocLeft, op "ne" OpStrNe AssocLeft]
        ,[tryop "<=" (OpLte) AssocLeft, tryop ">=" (OpGte) AssocLeft]
        ,[op1 '<' OpLt AssocLeft, op1 '>' OpGt AssocLeft]
        ,[op "&&" OpAnd AssocLeft, op "||" OpOr AssocLeft]
        ,[prefix '!' TNot]
     ]
   where
     op s f assoc = Infix (do{ symbol s; return (TOp f)}) assoc
     op1 s f assoc = Infix (do{ schar s; return (TOp f)}) assoc
     tryop s f assoc = Infix (do{ try(symbol s); return (TOp f)}) assoc
     prefix c f = Prefix (do { schar c; return f})

factor = nested <|> numval <|>  boolval <|> mystr <|> myvar <|> myfun <?> "term"
 where
  nested = do
    schar '('
    x <- myexpr
    schar ')'
    return x

neg = (char '-' >> return True)
     <|> (char '+' >> return False)
     <|> return False

numval = do n <- neg
            let adj :: Num n => n -> n
                adj v = if n then negate v else v
            iorf <- intOrFloat
            return $ case iorf of
                      Left  i -> tInt (adj (fromIntegral i))
                      Right f -> tFloat (adj f)


mystr = do s <- stringLit
           return $ tStr s
        <?> "string"

boolval = do
  b <- ((s "true" <|> try (s "on")) >> return True)
       <|> ((s "false" <|> s "off") >> return False)
  return . TVal $ T.fromBool b
 where s str = symbol str

myvar = do char '$'
           s <- identifier
           return $ TVar s
        <?> "variable"

myfun = do s <- identifier
           schar '('
           inner <- myexpr `sepBy` (char ',')
           schar ')'
           return $ TFun s inner
