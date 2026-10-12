module GoPro.Interactive where

import           Effectful
import           Effectful.Reader.Dynamic
import           GoPro.Commands
import           GoPro.DB
import           GoPro.Logging

-- | A convenience function for performing GoPro actions from within GHCI.
interactively :: String -> (forall es. [IOE, DB, LogFX, Reader Options] :>> es => Eff es a) -> IO a
interactively dbp = runWithOptions defaultOptions{optDBPath=dbp}
