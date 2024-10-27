module GoPro.Interactive where

import           Cleff
import           Cleff.Reader
import           GoPro.Commands
import           GoPro.DB
import           GoPro.Logging

-- | A convenience function for performing GoPro actions from within GHCI.
interactively :: String -> (forall es. [IOE, DatabaseEff, LogFX, Reader Options] :>> es => Eff es a) -> IO a
interactively dbp = runWithOptions defaultOptions{optDBPath=dbp}
