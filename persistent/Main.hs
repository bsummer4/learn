{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude.Unicode
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Time (UTCTime)
import Data.String (fromString)
import Data.List (concat,intersperse)

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

connStr = fromString $ concat $ intersperse " " $
	[ "dbname=tutorial"
	, "host=localhost"
	, "user=tutorial"
	, "password=tutorial"
	, "port=5432"
	]

main :: IO ()
main = withPostgresqlPool connStr 10 $ \pool ->
 flip runSqlPersistMPool pool $ do
		printMigration migrateAll
		runMigration migrateAll
		johnId <- insert $ User "John Doe" $ Just 35
		janeId <- insert $ User "Jane Doe" Nothing
		insert $ BlogPost "My fr1st p0st" johnId
		insert $ BlogPost "One more for good measure" johnId
		selectList [BlogPostAuthorId==.johnId] [LimitTo 1] >>=
			liftIO ∘ (print∷[Entity BlogPost]→IO())
		get johnId >>= liftIO ∘ (print ∷ Maybe User→IO())
