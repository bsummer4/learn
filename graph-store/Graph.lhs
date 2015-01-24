A Proposal For A More Efficient SourceGraph Representation

> {-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

> import ClassyPrelude hiding (hash)
> import Data.Word
> import Data.MultiMap (MultiMap)
> import Network.URI (URI)
> import qualified Data.Vector as Vector
> import qualified Data.ByteString.Lazy as LBS
> import qualified Data.ByteString as BS
> import Crypto.Hash (SHA1,hashlazy,Digest,digestFromByteString)
> import Data.Time (UTCTime)
> import Data.Binary (decode,encode,Binary(..),getWord8,putWord8)
> import Data.Byteable (toBytes)

> import qualified Data.Set as S
> import qualified Data.Text as T
> import qualified Data.Map as M

> newtype SHA1Sum = SHA1Sum (Digest SHA1)
>   deriving (Ord,Eq)

> fromJust :: Maybe a -> a
> fromJust Nothing = error "fromJust called with Nothing!"
> fromJust (Just a) = a

> instance Binary a => Binary (Vector a) where
>   put = put . Vector.toList
>   get = Vector.fromList <$> get

> instance Binary UTCTime where
>   put = undefined
>   get = undefined

> instance Binary URI where
>   put = undefined
>   get = undefined

> instance Binary SHA1Sum where
>   get = SHA1Sum <$> fromJust <$> digestFromByteString <$> BS.pack <$> replicateM 20 getWord8
>   put (SHA1Sum x) = mapM_ putWord8 $ BS.unpack $ toBytes x

> type HashOf a = SHA1Sum
> type ShaMap a     = Map (HashOf a) a

First, let's define a clean set of data types for definitions and
references within a single commit.

> type Loc          = (Text, Word32, Word32)
> type Def          = Loc
> type Defs         = Vector Loc
> type DefID        = Word32
> type Ref          = (Loc,DefID)
> type InternalRefs = Vector Ref

This let's us graph references and definitions within a repo, but we also
want references to external repos. We use the following tricks to make
lookups efficient:

  - Since both git and mercurial use SHA1 hashes to address revisions,
    we don't need to use string-based keys (URIs) to address commits.
  - References always refer to a build (not a repo), so we can refer to refs
    by an integer id, instead of needing to use an abstract string reference.
    This also means that there are never any broken links.

> type BuildInfo    = (SHA1Sum,URI,UTCTime)
> type XRef         = (BuildID,Ref)
> type ExternalRefs = Vector XRef
> data Build        = Build BuildInfo Defs InternalRefs ExternalRefs
> type BuildID      = HashOf Build

> instance Binary Text where
>   put = put . T.unpack
>   get = T.pack <$> get

> instance Binary Build where
>   put (Build i d r e) = put (i,d,r,e)
>   get = do { (i,d,r,e) <- get; return (Build i d r e) }

Now we need a way to store all of the builds and the references between them in
a database. We can just store all of the builds in a large content-addressable
map, but we also need a quick way to find references between repos.

> type Builds       = ShaMap Build
> type XRefs        = Set ((BuildID,Loc),(BuildID,DefID))
> type KnownRepos   = MultiMap SHA1Sum URI
> type BuildHistory = [BuildID]
> data DB           = DB Builds XRefs KnownRepos

This concrete Haskell representation are not very efficient, but think of
them as a schema for an efficient on-disk data structure. These data-types
have some very nice properties for efficient implementation.

- Builds, XRefs are all append-only tables.

- XRefs should be compact/fast enough to be handled by a single node.
  - mmap() friendly layout means no loading/parsing of files.
  - This is also deriviable from the Builds table.
    - If we lose data, we can efficiently reconstruct it from a snapshot,
      - BuildHistory has all the info we need for easy reconstruction.

- Builds is keyed on an SHA1 hash, which leads to efficient lookups/inserts.
  - Hash tables will have basically zero collisions.
    - SHA1-based hashing means keys always have a uniform distribution.
    - Hashing takes is basically free, just take some bits from the full
      sha1 hash.

Here's a quick reference implementation, so that I can play with this model:

> type Tarball = ByteString

> sha1 :: Binary a => a -> SHA1Sum
> sha1 = SHA1Sum . hashlazy . encode

> sha1Insert :: Binary a => a -> Map SHA1Sum a -> Map SHA1Sum a
> sha1Insert v = M.insert (sha1 v) v

> xrefs :: BuildID -> Build -> Set ((BuildID,Loc),(BuildID,DefID))
> xrefs from (Build _ _ _ rs) =
>   S.fromList $ (\(to,(loc,def)) -> ((from,loc),(to,def))) <$> Vector.toList rs

> insert :: DB → Build → DB
> insert db@(DB builds xrs repos) build = DB builds' xrs' repos
>   where buildID = sha1 build
>         xrs'    = xrefs buildID build
>         builds' = M.insert buildID build builds

-- fetch :: DB → SHA1Sum → IO (Maybe Tarball)
-- build :: Tarball → IO (Maybe Build)