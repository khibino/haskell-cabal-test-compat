{-# LANGUAGE CPP #-}

-- |
-- Module      : Distribution.TestSuite.Compat
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides subset of compatibility interface names
-- for Cabal older than 1.16.
module Distribution.TestSuite.Compat (prop, TestList, testList) where

import Test.QuickCheck (Testable, quickCheckResult, Result (Success))

import Control.Exception (try)
import Control.Applicative ((<$>))

#if MIN_VERSION_Cabal(1,16,0)

import Distribution.TestSuite
  (Test (Test), TestInstance (TestInstance), Result (Pass, Fail, Error), Progress (Finished))


simpleInstance :: IO Progress -> String -> Test
simpleInstance p name = Test this  where
  this = TestInstance p name [] [] (\_ _ -> Right this)

suite :: IO (Either String ()) -> String -> Test
suite t = simpleInstance $ do
  er <- try t
  return . Finished $ case er of
    Right (Right ()) -> Pass
    Right (Left m)   -> Fail m
    Left e           -> Error $ show (e :: IOError)

-- | Interface to make 'Test'.
prop :: Testable prop => prop -> String -> Test
prop t = suite $ qcEither <$> quickCheckResult t

-- | Interface type of 'Test' list to export.
type TestList = IO [Test]

-- | Convert interface into 'Test' list to export.
testList :: [Test] -> TestList
testList =  return

#else

import Distribution.TestSuite
  (TestOptions (..), Options (..), ImpureTestable (..), impure,
   Test, Result (Pass, Fail, Error))
import qualified Distribution.TestSuite as TestSuite


test114 :: IO (Either String ()) -> IO TestSuite.Result
test114 t = do
  er <- try t
  return $ case er of
    Right (Right ()) -> Pass
    Right (Left m)   -> Fail m
    Left e           -> Error $ show (e :: IOError)

prop114 :: Testable prop => prop -> IO TestSuite.Result
prop114 t = test114 $ qcEither <$> quickCheckResult t

data Suite114 t = Suite114 String t

instance TestOptions (Suite114 prop) where
  name (Suite114 n _) = n
  options = const []
  defaultOptions = const . return $ Options []
  check _ _ = []

-- instance ImpureTestable (Suite114 (IO (Either String ()))) where
--   runM (Suite114 _ t) _ = test114 t

instance Testable prop => ImpureTestable (Suite114 prop) where
  runM (Suite114 _ t) _ = prop114 t

-- -- | Interface to make 'Test'.
-- suite :: IO (Either String ()) -> String -> Test
-- suite t n = impure $ Suite114 n t

-- | Interface to make 'Test'.
prop :: Testable prop => prop -> String -> Test
prop t n = impure $ Suite114 n t


-- | Interface type of 'Test' list to export.
type TestList = [Test]

-- | Convert interface into 'Test' list to export.
testList :: [Test] -> TestList
testList =  id

#endif

qcEither :: Test.QuickCheck.Result -> Either String ()
qcEither =  d  where
  d (Success {}) = Right ()
  d x            = Left $ show x
