{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Paradox.MemoTest where

import Test.Tasty
import Test.Tasty.TH

import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import GHC.Generics (Generic)
import Paradox.Memo.Internal

import Data.Hashable
import Control.Concurrent.MVar

data Req = Req {
         v :: Int
    } deriving (Eq, Generic, Show)

instance Hashable Req

instance FuzzyKey Req where
    fkey (Req v') = Req { v = round $ fromIntegral (v' `div` 10) * (10 :: Double) }
    expiry _ = 0


data Unlock = Unlock deriving (Show, Eq)
data ThreadComplete = ThreadComplete | ThreadCancel deriving (Show, Eq)

case_ensureCachedDataReturned :: Assertion
case_ensureCachedDataReturned = do
    waitOnThing <- newEmptyMVar
    waitOnThreads <- newEmptyMVar
    memoizer <- fuzzyMemo' (\case
        Delete -> assertEqual "Value gets deleted" Unlock =<< takeMVar waitOnThing
        ThreadFinished -> putMVar waitOnThreads ThreadComplete
        ThreadCancelled -> putMVar waitOnThreads ThreadCancel
        _ -> return ()
        )
        id
    let k = Req 1
        functionToCache (Req x) = return x
        functionWontBeCalled (Req x) = return $ x + 1
        functionWillBeCalled (Req x) = return $ x + 2
    expectedValue <- memoizer functionToCache k

    assertEqual "Function was called and returned expected value" (1 :: Int) expectedValue

    cachedValue <- memoizer functionWontBeCalled k

    assertEqual "Function wasn't called and returned cached value" (1 :: Int) cachedValue

    putMVar waitOnThing Unlock

    cancelledThreadLock <- takeMVar waitOnThreads
    assertEqual "Thread cancelled from cache hit" ThreadCancel cancelledThreadLock

    finishedThreadLock <- takeMVar waitOnThreads
    assertEqual "Thread finished from cache expiry" ThreadComplete finishedThreadLock

    cacheMissFunctionValue <- memoizer functionWillBeCalled k

    assertEqual "Function was called because cache expired" (3 :: Int) cacheMissFunctionValue

    return ()

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
