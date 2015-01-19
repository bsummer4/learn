{-# LANGUAGE Arrows, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Prelude hiding (sum)

import           Opaleye (Column, Nullable, matchNullable, isNull,
                         Table(Table), required, queryTable,
                         Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                         (.++), ifThenElse, pgString, aggregate, groupBy,
                         count, avg, sum, leftJoin, runQuery,
                         showSqlForPostgres, Unpackspec,
                         PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool)

import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Calendar (Day)

import           Control.Arrow (returnA, (<<<))

import qualified Database.PostgreSQL.Simple as PGS

personTable :: Table (Column PGText, Column PGInt4, Column PGText)
                     (Column PGText, Column PGInt4, Column PGText)
personTable = Table "personTable" (p3 ( required "name"
                                      , required "age"
                                      , required "address" ))

personQuery :: Query (Column PGText, Column PGInt4, Column PGText)
personQuery = queryTable personTable

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres

main = printSql personQuery