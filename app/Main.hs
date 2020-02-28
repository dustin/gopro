module Main where

import           Control.Exception      (bracket_)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (ReaderT (..), asks, runReaderT)
import qualified Data.Set               as Set
import           Options.Applicative    (Parser, argument, execParser, fullDesc,
                                         help, helper, info, long, metavar,
                                         progDesc, showDefault, some, str,
                                         strOption, value, (<**>))
import           System.IO              (hFlush, hGetEcho, hSetEcho, stdin,
                                         stdout)

import           GoPro
import           GoPro.AuthDB
import           GoPro.DB

data Options = Options {
  optDBPath :: String,
  optArgv   :: [String]
  }

data Env = Env {
  gpOptions :: Options,
  gpToken   :: String
  }

type GoPro = ReaderT Env IO

options :: Parser Options
options = Options
  <$> strOption (long "dbpath" <> showDefault <> value "gopro.db" <> help "db path")
  <*> some (argument str (metavar "cmd args..."))

runSync :: GoPro ()
runSync = do
  tok <- asks gpToken
  db <- asks (optDBPath . gpOptions)
  seen <- Set.fromList <$> loadMediaIDs db
  l <- filter ((`Set.notMember` seen) . _media_id) <$> listWhile tok (listPred seen)
  liftIO $ print l
  storeMedia db l

    where listPred seen = all ((`Set.notMember` seen) . _media_id)

runAuth :: GoPro ()
runAuth = do
  liftIO (prompt "Enter email: ")
  u <- liftIO getLine
  p <- liftIO getPass
  db <- asks (optDBPath . gpOptions)
  res <- authenticate u p
  updateAuth db res

  where
    prompt x = putStr x >> hFlush stdout
    withEcho echo action = do
      prompt "Enter password: "
      old <- hGetEcho stdin
      bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

    getPass = withEcho False getLine

runReauth :: GoPro ()
runReauth = do
  db <- asks (optDBPath . gpOptions)
  a <- loadAuth db
  res <- refreshAuth a
  updateAuth db res

run :: String -> GoPro ()
run "auth"   = runAuth
run "reauth" = runReauth
run "sync"   = runSync
run x        = fail ("unknown command: " <> x)

main :: IO ()
main = do
  o@Options{..} <- execParser opts
  tok <- loadToken optDBPath
  runReaderT (run (head optArgv)) (Env o tok)

  where
    opts = info (options <**> helper)
           ( fullDesc <> progDesc "GoPro cloud utility.")
