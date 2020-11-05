module GoPro.Interactive where

import           GoPro.Commands

-- | A convenience function for performing GoPro actions from within GHCI.
interactively :: String -> GoPro a -> IO a
interactively dbp = runWithOptions (Options dbp "static" True 3 11 [""])
