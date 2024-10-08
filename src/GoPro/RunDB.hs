module GoPro.RunDB where

import           Cleff
import           Data.List         (isPrefixOf)
import           GoPro.DB
import           GoPro.DB.Postgres (runDatabasePostgresStr)
import           GoPro.DB.Sqlite   (runDatabaseSqliteStr)

withDB :: IOE :> es => String -> Eff (DatabaseEff : es) a -> Eff es a
withDB s
  | "postgres:" `isPrefixOf` s = runDatabasePostgresStr s
  | otherwise = runDatabaseSqliteStr s
