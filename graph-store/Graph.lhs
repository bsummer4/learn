A Proposal For A More Efficient SourceGraph Representation

> {-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

> import ClassyPrelude
> import Data.Word
> import Data.MultiMap (MultiMap)
> import Network.URI (URI)
> import qualified Data.Vector.Unboxed as Vec
> import qualified Crypto.Hash.SHA1 as SHA1
> import Data.Time (UTCTime)

> type SHA1 = SHA1.Ctx

First, let's define a clean set of data types for definitions and
references within a single commit.

> data Loc          = Loc {fn∷FilePath, start∷Word32, length∷Word32}
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

> data BuildInfo    = BuildInfo SHA1 URI UTCTime
> type XRef         = (Loc,BuildID,DefID)
> type ExternalRefs = Vector XRef
> type Build        = (BuildInfo, Defs, InternalRefs, ExternalRefs)
> type BuildID      = SHA1 -- Hash of a build.

Now we need a way to store all of the builds and the references between them in
a database. We can just store all of the builds in a large content-addressable
map, but we also need a quick way to find references between repos.

> type Builds       = Map BuildID Build
> type XRefsFrom    = MultiMap BuildID (XRef,BuildID,DefID)
> type XRefsTo      = MultiMap BuildID (XRef,BuildID,DefID)
> type KnownRepos   = MultiMap SHA1 URI
> type BuildHistory = [BuildID]
> data DB           = DB Builds XRefsFrom XRefsTo KnownRepos

> type Tarball = ByteString
> type InsertOp = DB → Build → DB
> type FetchOp = DB → SHA1 → IO (Maybe Tarball)
> type BuildOp = Tarball → IO (Maybe Build)

This concrete representation is not very efficient, but these datatype
have some very nice properties that can lead to very efficient
implementations. Specifically,

- Builds, XRefsFrom, and XRefsTo are both append-only tables.

- XRefsFrom,XRefsTo should be compact/fast enough to be handled by a single node.
  - mmap() friendly layout means no loading/parsing of files.
  - This is also deriviable from the Builds table.
    - If we lose data, we can efficiently reconstruct it from a snapshot,
      - BuildHistory has all the info we need for easy reconstruction.

- Builds is keyed on an SHA1 hash, which leads to efficient lookups/inserts.
  - Hash tables will have basically zero collisions.
    - SHA1-based hashing means keys always have a uniform distribution.
    - Hashing takes is basically free, just take some bits from the full
      sha1 hash.
