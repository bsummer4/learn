{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- Solen from:
-- https://github.com/lassoinc/postgis/blob/master/Database/PostgreSQL/Postgis.hs

module Geo where

import Database.Persist
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.Text
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.String (fromString)

data Geo = Geo ByteString deriving Show

instance PersistField Geo where
	toPersistValue (Geo t) = PersistDbSpecific t
	fromPersistValue (PersistDbSpecific t) = Right $ Geo $
		Data.ByteString.concat ["'", t, "'"]
	fromPersistValue _ =
		Left "Geo values must be converted from PersistDbSpecific"

instance PersistFieldSql Geo where
	sqlType _ = SqlOther "GEOGRAPHY(POINT,4326)"

toPoint :: Double -> Double -> Geo
toPoint lat lon = Geo $ Data.ByteString.concat
	["'POINT(", ps $ lon, " ", ps $ lat, ")'"]
		where ps = fromString . show
