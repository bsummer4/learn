module Common
    ( RetVal, TclM
    , TclState
    , Err(..)
    , TclCmd, applyTo, cmdBody, getOrigin
    , runTclM
    , makeState
    , withLocalScope
    , withNS
    , makeCmdMap
    , mergeCmdMaps
    , getProc
    , getProcNS
    , regProc
    , varGetNS
    , varGet
    , varModify
    , varSet
    , varSetHere
    , varExists
    , varUnset
    , varUnsetNS
    , renameProc
    , getArray
    , addChan
    , removeChan
    , getChan
    , evtAdd
    , evtGetDue
    , uplevel
    , upvar
    , makeEnsemble
    , io
    , tclErr
    , treturn
    , ret
    , argErr
    , stackLevel
    , globalVars
    , localVars
    , currentVars
    , currentNS
    , parentNS
    , existsNS
    , deleteNS
    , childrenNS
    , variableNS
    , exportNS
    , getExportsNS
    , importNS
    , checkpoint
    , commandNames
    ) where

import qualified Data.ByteString.Char8 as B
import Control.Monad.Error
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.IORef
import Data.Unique

import qualified EventMgr as Evt

import qualified TclObj as T
import TclChan
import VarName
import Util

--------------------------------------------------------------------------------------------------------------

type RetVal = T.TclObj

data Err = ERet !RetVal | EBreak
          | EContinue | EDie String deriving (Eq,Show)

instance Error Err where
 noMsg    = EDie "An error occurred."
 strMsg s = EDie s

type TclM = ErrorT Err (StateT TclState IO)

checkpoint str f = f `catchError` handle
 where handle (EDie es) = throwError (EDie (str ++ es))
       handle e         = throwError e

data Namespace = TclNS {
         nsName :: BString,
         nsProcs :: CmdMap,
         nsFrame :: FrameRef,
         nsExport :: [BString],
         nsParent :: Maybe NSRef,
         nsChildren :: Map.Map BString NSRef }


type FrameRef = IORef TclFrame
type NSRef = IORef Namespace

data TclFrame = TclFrame {
      frVars :: !VarMap,
      upMap :: Map.Map BString (FrameRef,BString),
      frNS :: NSRef,
      frTag :: Int  }

type TclStack = [FrameRef]

data TclState = TclState {
    tclChans :: ChanMap,
    tclEvents :: Evt.EventMgr T.TclObj,
    tclStack :: !TclStack,
    tclGlobalNS :: !NSRef }

type TclCmd = [T.TclObj] -> TclM T.TclObj
data TclCmdObj = TclCmdObj {
                   cmdName :: BString,
                   cmdBody :: BString,
                   cmdOrigNS :: Maybe BString,
                   cmdAction :: TclCmd }

type ProcKey = BString
data CmdMap = CmdMap {
      cmdMapEpoch :: !Int,
      unCmdMap :: !(Map.Map ProcKey TclCmdObj)
  }

type TclArray = Map.Map BString T.TclObj
data TclVar = ScalarVar !T.TclObj | ArrayVar TclArray | Undefined deriving (Eq,Show)
type VarMap = Map.Map BString TclVar


getOrigin p = if nso == nsSep
                  then return $ B.append nso pname
                  else return $ B.concat [nso, nsSep, pname]
 where nso = maybe nsSep id (cmdOrigNS p)
       pname = cmdName p

applyTo !(TclCmdObj _ _ _ !f) !args = f args
{-# INLINE applyTo #-}

mkProcAlias nsr pn = do
    pr <- getProcNorm pn nsr
    case pr of
      Nothing -> fail "trying to import proc that doesn't exist"
      Just p  -> return $ p { cmdAction = inner }
 where inner args = do thep <- getProcNorm pn nsr
                       case thep of
                        Nothing -> tclErr "bad imported command. Yikes"
                        Just p  -> p `applyTo` args

globalNS fr = do
  return $ TclNS { nsName = nsSep,
                   nsProcs = emptyCmdMap, nsFrame = fr,
                   nsExport = [],
                   nsParent = Nothing, nsChildren = Map.empty }

makeCmdMap :: [(String,TclCmd)] -> CmdMap
makeCmdMap = CmdMap 0 . Map.fromList . map toTclCmdObj . mapFst pack

toTclCmdObj (n,v) = (n, TclCmdObj n errStr Nothing v)
 where errStr = pack $ show n ++ " isn't a procedure"

tclErr :: String -> TclM a
tclErr s = do
  (setErrorInfo s) `orElse` ret
  throwError (EDie s)

setErrorInfo s = do
  glFr <- getGlobalNS >>= getNSFrame
  varSet' (VarName (pack "errorInfo") Nothing) (T.mkTclStr s) glFr

mergeCmdMaps :: [CmdMap] -> CmdMap
mergeCmdMaps = CmdMap 0 . Map.unions . map unCmdMap

makeVarMap = Map.fromList . mapSnd ScalarVar

makeState :: [(BString,T.TclObj)] -> CmdMap -> IO TclState
makeState = makeState' baseChans

makeState' :: ChanMap -> [(BString,T.TclObj)] -> CmdMap -> IO TclState
makeState' chans vlist procs = do
    fr <- createFrame (makeVarMap vlist)
    gns <- globalNS fr
    ns <- newIORef (gns { nsProcs = procs })
    setFrNS fr ns
    addChildNS ns (pack "") ns
    return $! TclState { tclChans = chans,
                         tclEvents = Evt.emptyMgr,
                         tclStack = [fr],
                         tclGlobalNS = ns }

getStack = gets tclStack
{-# INLINE getStack  #-}
getNsCmdMap nsr = (io . readIORef) nsr >>= \v -> return $! (nsProcs v)
{-# INLINE getNsCmdMap #-}

putStack s = modify (\v -> v { tclStack = s })
{-# INLINE putStack  #-}
modStack :: (TclStack -> TclStack) -> TclM ()
modStack f = modify (\v -> v { tclStack = f (tclStack v) })
{-# INLINE modStack #-}
getFrame = do st <- gets tclStack
              case st of
                 (fr:_) -> return $! fr
                 _      -> tclErr "stack badness"
{-# INLINE getFrame  #-}

io :: IO a -> TclM a
io = liftIO
{-# INLINE io #-}

stackLevel = getStack >>= return . pred . length
globalVars = getGlobalNS >>= getNSFrame >>= getFrameVars >>= return . Map.keys
localVars = getFrame >>= getFrameVars >>= return . Map.keys
currentVars = do f <- getFrame
                 vs <- getFrameVars f
                 mv <- getUpMap f
                 return $ Map.keys vs ++ Map.keys mv

commandNames = getCurrNS >>= getNsCmdMap >>= return . Map.keys . unCmdMap



argErr s = tclErr ("wrong # of args: " ++ s)

runTclM :: TclM a -> TclState -> IO (Either Err a, TclState)
runTclM code env = runStateT (runErrorT code) env

onChan f = gets tclChans >>= f
modChan f = modify (\s -> s { tclChans = f (tclChans s) })
getChan n = onChan (\m -> return (lookupChan n m))
addChan c    = modChan (insertChan c)
removeChan c = modChan (deleteChan c)

evtAdd e t = do
  em <- gets tclEvents
  (tag,m) <- io $ Evt.addEvent e t em
  modify (\s -> s { tclEvents = m })
  treturn tag

evtGetDue = do
  em <- gets tclEvents
  (d,em') <- io $ Evt.getDue em
  when (not (null d)) $ modify (\s -> s { tclEvents = em' })
  return d

upped !s !fr = getUpMap fr >>= \f -> return $! (Map.lookup s f)
{-# INLINE upped #-}

getProc !pname = getProcNS (parseProc pname)

-- TODO: Special case for globals and locals when we're in the global NS?
getProcNS (NSQual nst n) = do
  res <- (getNamespace nst >>= getProcNorm n) `ifFails` Nothing
  if isNothing res && not (isGlobalQual nst)
    then do ns2 <- if noNsQual nst then getGlobalNS else getNamespace (asGlobal nst)
            getProcNorm n ns2
    else return $! res
 where
  isNothing Nothing = True
  isNothing _       = False
{-# INLINE getProcNS #-}

getProcNorm :: ProcKey -> NSRef -> TclM (Maybe TclCmdObj)
getProcNorm !i !nsr = do
  currpm <- getNsCmdMap nsr
  return $! (pmLookup i currpm)
{-# INLINE getProcNorm #-}

pmLookup :: ProcKey -> CmdMap -> Maybe TclCmdObj
pmLookup !i !m = Map.lookup i (unCmdMap m)
{-# INLINE pmLookup #-}

rmProc name = rmProcNS (parseProc name)
rmProcNS (NSQual nst n) = getNamespace nst >>= rmFromNS
 where rmFromNS nsref = io $ changeProcs nsref (Map.delete n)


regProc name body pr =
    let (NSQual nst n) = parseProc name
    in regProcNS nst n (TclCmdObj n body Nothing pr)

regProcNS nst k newProc = getNamespace nst >>= regInNS
 where
  pmInsert proc m = Map.insert k proc m
  regInNS nsr = do fn <- nsr `refExtract` nsName
                   io $ changeProcs nsr (pmInsert (setOrigin fn newProc))
                   return ()
  setOrigin fn x              = if cmdOrigNS x == Nothing then x { cmdOrigNS = Just fn } else x

varSet :: BString -> T.TclObj -> TclM RetVal
varSet !n v = varSetNS (parseVarName n) v
{-# INLINE varSet #-}

varSetNS qvn v = usingNsFrame qvn (\n f -> varSet' n v f)
{-# INLINE varSetNS #-}

varSetHere vn v = getFrame >>= varSet' vn v

varSet' vn v frref = do
     isUpped <- upped (vnName vn) frref
     case isUpped of
         Nothing    -> modVar (vnName vn) >> return v
         Just (f,s) -> varSet' (vn {vnName = s}) v f
 where cantSetErr why = fail $ "can't set " ++ showVN vn ++ ":" ++ why
       modVar str = do
         vm <- getFrameVars frref
         let changeVar = insertVar frref str
         case vnInd vn of
           Nothing -> case Map.lookup str vm of
                        Just (ArrayVar _) -> cantSetErr "variable is array"
                        _                 -> changeVar (ScalarVar v)
           Just i  -> case Map.findWithDefault (ArrayVar Map.empty) str vm of
                        ArrayVar prev ->  changeVar (ArrayVar (Map.insert i v prev))
                        Undefined     ->  changeVar (ArrayVar (Map.singleton i v))
                        _     -> cantSetErr "variable isn't array"


varModify :: BString -> (T.TclObj -> TclM T.TclObj) -> TclM RetVal
varModify !n f = do
  let vn = parseVarName n
  val <- varGetNS vn
  res <- f val
  varSetNS vn res
{-# INLINE varModify #-}

varExists :: BString -> TclM Bool
varExists name = (varGet name >> return True) `catchError` (\_ -> return False)

renameProc old new = do
  mpr <- getProc old
  case mpr of
   Nothing -> tclErr $ "can't rename, bad command " ++ show old
   Just pr -> do rmProc old
                 unless (bsNull new) (regProc new (cmdBody pr) (cmdAction pr))

varUnset :: BString -> TclM RetVal
varUnset name = varUnsetNS (parseVarName name)

varUnsetNS :: NSQual VarName -> TclM RetVal
varUnsetNS qns = usingNsFrame qns varUnset'

usingNsFrame :: NSQual VarName -> (VarName -> FrameRef -> TclM RetVal) -> TclM RetVal
usingNsFrame (NSQual !ns !vn) f = lookupNsFrame ns >>= f vn
 where lookupNsFrame Nothing = getFrame
       lookupNsFrame ns = getNamespace ns >>= getNSFrame
{-# INLINE usingNsFrame #-}

{- This specialization is ugly, but GHC hasn't been doing it for me and it
 - knocks a few percent off the runtime of my benchmarks. -}
usingNsFrame2 :: NSQual BString -> (BString -> FrameRef -> TclM b) -> TclM b
usingNsFrame2 (NSQual !ns !vn) f = lookupNsFrame ns >>= f vn
 where lookupNsFrame Nothing = getFrame
       lookupNsFrame ns  = getNamespace ns >>= getNSFrame
{-# INLINE usingNsFrame2 #-}

varUnset' vn frref = do
     isUpped <- upped (vnName vn) frref
     case isUpped of
         Nothing    -> modVar >> ret
         Just (f,s) -> do
             when (not (isArr vn)) $ do
                 changeUpMap frref (Map.delete (vnName vn))
             varUnset' (vn {vnName = s}) f
 where
  noExist = cantUnset "no such variable"
  cantUnset :: Monad m => String -> m a
  cantUnset why = fail $ "can't unset " ++ showVN vn ++ ": " ++ why
  modVar = do
    vm <- getFrameVars frref
    let str = vnName vn
    let deleteVar = changeVars frref (Map.delete str)
    val <- maybe noExist return (Map.lookup str vm)
    case vnInd vn of
      Nothing -> deleteVar
      Just i  -> case val of
                   ArrayVar prev -> case Map.lookup i prev of
                                      Nothing -> cantUnset "no such element in array"
                                      Just _  -> insertVar frref str (prev `modArr` (Map.delete i))
                   ScalarVar _   -> cantUnset "variable isn't array"
                   _             -> noExist

modArr v f = ArrayVar (f v)

getArray :: BString -> TclM TclArray
getArray name = usingNsFrame2 (parseProc name) getArray'

getArray' :: BString -> FrameRef -> TclM TclArray
getArray' name frref = do
   var <- varLookup name frref
   case var of
      Just (ArrayVar a) -> return a
      Just _            -> tclErr $ "can't read " ++ show name ++ ": variable isn't array"
      Nothing           -> tclErr $ "can't read " ++ show name ++ ": no such variable"

varLookup !name !frref = do
   isUpped <- upped name frref
   case isUpped of
      Nothing    -> getFrameVars frref >>= \m -> return $! (Map.lookup name m)
      Just (f,n) -> varLookup n f
{-# INLINE varLookup #-}

varGet :: BString -> TclM RetVal
varGet !n = varGetNS (parseVarName n)

varGetNS :: NSQual VarName -> TclM RetVal
varGetNS qns = usingNsFrame qns varGet'
{-# INLINE varGetNS #-}

varGet' vn !frref = do
  var <- varLookup (vnName vn) frref
  case var of
   Nothing -> cantReadErr "no such variable"
   Just o  -> o `getInd` (vnInd vn)
 where cantReadErr why  = fail $ "can't read " ++ showVN vn ++ ": " ++ why
       getInd (ScalarVar o) Nothing = return o
       getInd (ScalarVar _) _       = cantReadErr "variable isn't array"
       getInd (ArrayVar o) (Just i) = maybe (cantReadErr "no such element in array") return (Map.lookup i o)
       getInd (ArrayVar _)  _       = cantReadErr "variable is array"
       getInd Undefined     _       = cantReadErr "no such variable"


uplevel :: Int -> TclM a -> TclM a
uplevel i p = do
  (curr,new) <- liftM (splitAt i) getStack
  when (null new) (tclErr ("bad level: " ++ show i))
  putStack new
  res <- p `ensure` (modStack (curr ++))
  return res
{-# INLINE uplevel #-}

getUpFrame i = do st <- getStack
                  if length st <= i
                      then fail "too far up the stack"
                      else return $! (st!!i)

linkToFrame name (upfr, upname) = do
  frref <- getFrame
  changeUpMap frref (Map.insert name (upfr, upname))

upvar n d s = do
   upfr <- getUpFrame n
   s `linkToFrame` (upfr, d)
{-# INLINE upvar #-}

deleteNS name = do
 nst <- parseNSTag name
 ns <- getNamespace' nst >>= readRef
 case nsParent ns of
   Nothing -> return ()
   Just p -> removeChild p (nsTail nst)

removeChild nsr child = io (modifyIORef nsr (\v -> v { nsChildren = Map.delete child (nsChildren v) } ))
addChildNS nsr name child = (modifyIORef nsr (\v -> v { nsChildren = Map.insert name child (nsChildren v) } ))

getNamespace Nothing    = getCurrNS
getNamespace (Just nst) = getNamespace' nst
{-# INLINE getNamespace #-}

-- TODO: Unify namespace getters
getNamespace' (NS gq nsl) = do
    base <- if gq then getGlobalNS else getCurrNS
    foldM getKid base nsl
 where getKid !nsref !k = do
          kids <- nsref `refExtract`  nsChildren
          case Map.lookup k kids of
             Nothing -> tclErr $ "can't find namespace " ++ show k ++ " in " ++ show nsl
             Just v  -> return $! v

getOrCreateNamespace (NS gq nsl) = do
    base <- if gq then getGlobalNS else getCurrNS
    foldM getKid base nsl
 where getKid nsref k = do
          kids <- nsref `refExtract` nsChildren
          case Map.lookup k kids of
             Nothing -> io (mkEmptyNS k nsref)
             Just v  -> return $! v

existsNS ns = (parseNSTag ns >>= getNamespace' >> return True) `catchError` (\_ -> return False)

variableNS name val = do
  let (NSQual ns (VarName n ind)) = parseVarName name
  ensureNotArr ind
  nsfr <- getNamespace ns >>= getNSFrame
  fr <- getFrame
  same <- sameTags fr nsfr
  if same then insertVar fr name varVal
          else n `linkToFrame` (nsfr, n)
 where
   ensureNotArr Nothing  = return ()
   ensureNotArr (Just _) = tclErr $ "can't define " ++ show name ++ ": name refers to value in array"
   varVal = maybe Undefined ScalarVar val
   sameTags f1 f2 = do
      t1 <- getTag f1
      t2 <- getTag f2
      return (t1 == t2)

exportNS clear name = do
  nsr <- getCurrNS
  io $ modifyIORef nsr (\n -> n { nsExport = (name:(getPrev n)) })
 where getPrev n = if clear then [] else nsExport n

getExportsNS = do
  getCurrNS >>= readRef >>= return . reverse . nsExport

importNS force name = do
    let (NSQual nst n) = parseProc name
    nsr <- getNamespace nst
    exported <- getExports nsr n
    mapM (importProc nsr) exported
    return . T.mkTclList . map T.mkTclBStr $ exported
 where importProc nsr n = do
            np <- mkProcAlias nsr n
            when (not force) $ do
                 oldp <- getProcNS (NSQual Nothing n)
                 case oldp of
                    Nothing -> return ()
                    Just _  -> tclErr $ "can't import command " ++ show n ++ ": already exists"
            regProcNS Nothing n np
       getExports nsr pat = do
               ns <- readRef nsr
               let exlist = nsExport ns
               let pnames = Map.keys (unCmdMap (nsProcs ns))
               let filt = filter (\v -> or (map (`globMatch` v) exlist)) pnames
               return (globMatches pat filt)


getTag frref = do
  f <- readRef frref
  return (frTag f)

setFrNS !frref !nsr = modifyIORef frref (\f -> f { frNS = nsr })

withLocalScope :: [(BString, T.TclObj)] -> TclM b -> TclM b
withLocalScope vl f = do
    ns <- getCurrNS
    fr <- io $! createFrameWithNS ns $! makeVarMap vl
    withScope fr f
{-# INLINE withLocalScope #-}

withScope :: FrameRef -> TclM a -> TclM a
withScope !frref fun = do
  stack <- getStack
  -- when (length stack > 10000) (tclErr $ "Stack too deep: " ++ show 10000)
  putStack $ frref : stack
  fun `ensure` (modStack (drop 1))

mkEmptyNS name parent = do
    pname <- liftM nsName (readIORef parent)
    emptyFr <- createFrame emptyVarMap
    let sep = if pname == nsSep then B.empty else nsSep
    let fullname = B.concat [pname, sep, name]
    new <- newIORef $ TclNS { nsName = fullname,
                              nsProcs = emptyCmdMap, nsFrame = emptyFr,
                              nsExport = [],
                              nsParent = Just parent, nsChildren = Map.empty }
    addChildNS parent name new
    setFrNS emptyFr new
    return $! new

withNS :: BString -> TclM a -> TclM a
withNS name f = do
     newCurr <- parseNSTag name >>= getOrCreateNamespace
     withExistingNS f newCurr

withExistingNS f !nsref = do
  fr <- getNSFrame nsref
  withScope fr f

getFrameVars :: FrameRef -> TclM VarMap
getFrameVars !frref = (frref `refExtract` frVars) >>= \r -> return $! r
{-# INLINE getFrameVars #-}

getUpMap !frref = (frref `refExtract` upMap) >>= \r -> return $! r
{-# INLINE getUpMap #-}

getNSFrame :: NSRef -> TclM FrameRef
getNSFrame !nsref = nsref `refExtract` nsFrame


getCurrNS = getFrame >>= liftIO . readIORef >>= \f -> return $! (frNS f)
{-# INLINE getCurrNS #-}

getGlobalNS = gets tclGlobalNS

readRef :: IORef a -> TclM a
readRef !r = (liftIO . readIORef) r
{-# INLINE readRef #-}

refExtract !ref !f = readRef ref >>= \d -> let r = f d in r `seq` (return $! r)
{-# INLINE refExtract #-}

currentNS = getCurrNS >>= readRef >>= return . nsName

parentNS = do
 ns <- getCurrNS >>= readRef
 case nsParent ns of
   Nothing -> return B.empty
   Just v  -> readRef v >>= return . nsName

childrenNS :: TclM [BString]
childrenNS = do
  ns <- getCurrNS >>= readRef
  (return . Map.keys . nsChildren) ns

ensure action p = do
   r <- action `catchError` (\e -> p >> throwError e)
   p
   return $! r

ret :: TclM RetVal
ret = return T.empty
{-# INLINE ret #-}

treturn :: BString -> TclM RetVal
treturn = return . T.mkTclBStr
{-# INLINE treturn #-}

createFrame !vref = do
   tag <- liftM hashUnique newUnique
   res <- newIORef $! TclFrame { frVars = vref, upMap = Map.empty, frTag = tag, frNS = undefined }
   return $! res

createFrameWithNS !nsref !vref = do
   tag <- liftM hashUnique newUnique
   res <- newIORef $! TclFrame { frVars = vref, upMap = Map.empty, frTag = tag, frNS = nsref }
   return $! res

changeUpMap fr fun = io (modifyIORef fr (\f -> f { upMap = fun (upMap f) }))

changeVars !fr fun = io (modifyIORef fr (\f -> let r = fun (frVars f) in r `seq` f { frVars = r } ))
{-# INLINE changeVars #-}

insertVar fr k v = changeVars fr (Map.insert k v)
{-# INLINE insertVar #-}

changeProcs nsr fun = do
           ns <- readIORef nsr
           modKids (\x -> changeProcs x id) ns
           writeIORef nsr (updateNS ns)
 where update (CmdMap e m) = CmdMap (e+1) (fun m)
       updateNS ns = ns { nsProcs = update (nsProcs ns) }

modKids f ns = let kidfun kr = do
                        kid <- readIORef kr
                        when (nsParent kid /= Nothing) (f kr)
              in mapM_ kidfun (Map.elems (nsChildren ns))

makeEnsemble name subs = top
  where top args = case args of
                   (x:xs) -> case Map.lookup (T.asBStr x) (unCmdMap subMap) of
                              Nothing -> tclErr $ "unknown subcommand " ++ show (T.asBStr x) ++ ": must be "
                                                     ++ commaList "or" (map unpack (cmdMapNames subMap))
                              Just f  -> (cmdAction f) xs
                   []  -> argErr $ " should be \"" ++ name ++ "\" subcommand ?arg ...?"
        subMap = makeCmdMap subs
        cmdMapNames = map cmdName . Map.elems . unCmdMap

emptyCmdMap = CmdMap 0 Map.empty
emptyVarMap = Map.empty
