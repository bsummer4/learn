{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, UnicodeSyntax #-}

import Prelude.Unicode
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
	User sql=user
		name String
		age Int Maybe
		UniqueString name
		deriving Show
	BlogPost sql=post
		title String
		authorId UserId
		deriving Show
	|]

main ∷ IO ()
main = runSqlite ":memory:" $ do
	runMigration migrateAll

	johnId <- insert $ User "John Doe" $ Just 35
	janeId <- insert $ User "Jane Doe" Nothing

	insert $ BlogPost "My fr1st p0st" johnId
	insert $ BlogPost "One more for good measure" johnId

	selectList [BlogPostAuthorId==.johnId] [LimitTo 1] >>=
		liftIO ∘ (print∷[Entity BlogPost]→IO())

	get johnId >>= liftIO ∘ (print ∷ Maybe User→IO())
