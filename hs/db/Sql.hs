{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple

hello = do
  conn <- connect defaultConnectInfo
  query conn "select 2 + 2"
