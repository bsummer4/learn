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
import Data.Time.Clock (getCurrentTime)
import Geo

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
	User sql=user
		name String
		age Int Maybe
		created UTCTime default=now()
		UniqueString name
		deriving Show
	BlogPost sql=post
		title String
		authorId UserId
		location Geo Maybe
		created UTCTime default=now()
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
main = do
	withPostgresqlPool connStr 10 $ \pool ->
		flip runSqlPersistMPool pool $ do
			printMigration migrateAll
			runMigration migrateAll
			now <- liftIO getCurrentTime
			johnId <- insert $ User "John Doe" (Just 35) now
			janeId <- insert $ User "Jane Doe" Nothing now
			insert $ BlogPost "My fr1st p0st" johnId Nothing now
			insert $ BlogPost "One more for good measure" johnId Nothing now
			selectList [BlogPostAuthorId==.johnId] [LimitTo 1] >>=
				liftIO ∘ (print∷[Entity BlogPost]→IO())
			get johnId >>= liftIO ∘ (print ∷ Maybe User→IO())
			return ()
