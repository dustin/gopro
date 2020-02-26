module Main where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (ReaderT (..), asks, runReaderT)

import           GoPro

data Options = Options

data Env = Env {
  options :: Options,
  gpToken :: String
  }

type GoPro = ReaderT Env IO

run :: GoPro ()
run = do
  tok <- asks gpToken
  (l, p) <- list tok 5 0
  liftIO $ print p
  mapM_ (liftIO . print) l

main :: IO ()
main = runReaderT run (Env Options mytoken)

  where
    mytoken = "eyJhbGciOiJSU0EtT0FFUCIsImVuYyI6IkExMjhHQ00ifQ.T16TR179vhgaLrXjnLPeNVZ7QpHftbKDTTFA8Fpy9xob1l8ye7NLZiNuSez5hlO781knO-W-oYFm94Z0Upid39jsYk1dcbgApmzPRzov8LRsxyqrAC3g_nuaqYQZ1Y1zBMSfIQiV-pWpTanHta95F6QleA4cYZ8RNDfBUyWuNR3puGucE9VLoYIF15-mBeEICRt8ToWYi0AkO2DIqNvvmanOFjOo6ADKywP9jf_tOWMdMt7L_lq-fwlEmGJ0PfUQFKoJrFeoHOJqfyLv6wwvtw83DWHAHcg8Y6fuA6fl4wyxqJT3FvRCrKPJdaj2faVv6DFsTGWUwey_vMTY2Acwvw.xcz0YhoHBvSteFG0._CI_9SNQ-a0rhgCBTPYNMPNlnz_xatJHav8d9CrtdZ3exWCfXqEF8GWSsZzw5oXCp-GxHLG-qgcc5S1QIcoDFNYsBo1_pQAGJ-5_whQ4nePKmDyQ_J26xMll0K-krkE576cCoAbh6q3f_OqvcgduDNoiOFsZce6bKszAa-VXsJkUpKmWm38YfWyiu9tkLzcZjBw_4Wy0Y7U88yyf-POV9MABquW4_nwmDog_HYx4KQywasqnWhM9GSJiwSZ017hJ_76TpPfVXJ_YaEXYQR51tjwpNGToMrFgg_UmhZYPVgTyd7P8ys6PK3ZAZRguN5OzOb-GHqgeFvNmm7rM6Bfws4Y1SKD9qjxn5FowVYmJz77jy2zA-7xcvX6ZhZH-hRkMnWgSOmL3o0HPFNawC3P4J12B_FYJMIKsR6CNdHmq6wJdFnP50IWkyzSDMyxyAikesfq99RTbzLGcpyN1n5B_iHkjzN1iDBueU1oYXO8WsdXxL_WwMHWV0Wt3CA2hvCZSPErsCWd7VD4_BFPvOcIR7AlR9iF-6WdEqfFBcC-yf4DDK2tjIT15NYnH6d-qUhy-ZxIrb9eHsJ-4iMEt53eVvxcJ4rl0X3Ds4AsBGEH43RvqXd_wwaTNQQdgpZnjFA5VIn84XlkjYDD6gK3umY6AFarIglQjHfiy9ekbKidgI939OsmB1ofMFFaXdfVEKKIcREhewvw.i8u4QDp3W7D58CiWcV18EQ"
