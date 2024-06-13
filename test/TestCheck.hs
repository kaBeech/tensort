module TestCheck (isPass, check) where

import Control.Monad (unless)
import System.Exit
import Test.QuickCheck

-- | Run a QuickCheck test and exit with a failure if it fails

-- | This is used so that the testing suite will fail if any QuickCheck tests
--   fail

-- | ==== __Examples__
--   >>> check (1 == 1)
--   ...
--   >>> check (1 == 2)
--   ...
--   ...exit with failure
check :: (Testable prop) => prop -> IO ()
check prop = do
  result <- quickCheckResult (withDiscardRatio 2000 prop)
  unless (isPass result) exitFailure

-- | Returns True if a test passes, and False otherwise

-- | ==== __Examples__
--   >>> isPass (Success {})
--   True
--   >>> isPass (GaveUp {})
--   False
--   >>> isPass (Failure {})
--   False
--   >>> isPass (_ {})
--   False
isPass :: Result -> Bool
isPass (Success {}) = True
isPass _ = False
