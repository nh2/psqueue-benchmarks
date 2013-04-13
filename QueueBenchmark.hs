{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Criterion.Main

import Data.PSQueue            as PSQ -- PSQueue
import Data.FingerTree.PSQueue as FT  -- fingertree-psqueue
import Data.Queue.PQueue       as QL  -- queuelike
import Data.Queue.Class        as QL
import GHC.Event.PSQ           as GHC
import GHC.Event.Unique        as GHC


-- Run with: ghc --make -O2 QueueBenchmark.hs && ./QueueBenchmark -o report.html

_N :: Int
_N = 10000 -- 100000

main :: IO ()
main = do
  randomNumbers :: [Int] <- take _N <$> getRandoms

  -- print $ makePSQ randomNumbers
  -- print $ toListPS $ makePSQ randomNumbers

  ghcUniqueSource <- GHC.newSource
  ghcUniques <- replicateM _N (GHC.newUnique ghcUniqueSource)

  let preparedPSQ = makePSQ randomNumbers :: PSQ.PSQ Int Double
      preparedFT  = makeFT randomNumbers  :: FT.PSQ Int Double
      preparedQL  = makeQL randomNumbers  :: QL.PQueue (Int :-> Double)
      preparedGHC = makeGHC ghcUniques randomNumbers :: GHC.PSQ Int


  putStrLn "Crash checks:"
  putStrLn $ "PSQ: " ++ findminPSQsequential 1000000 -- works with 5000000
  putStrLn $ "FT: " ++ findminFTsequential 100000 -- CRASHES with 1000000
  putStrLn $ "QL: " ++ findminQLsequential 1000000 -- works with 5000000
  let _GHC_CRASH_N = 100000 -- CRASHES with 1000000
  crashCheckUniques <- replicateM _GHC_CRASH_N (GHC.newUnique ghcUniqueSource)
  putStrLn $ "GHC: " ++ findminGHCsequential crashCheckUniques _GHC_CRASH_N


  defaultMain
    [ bgroup "PSQ" [ -- PSQueue stack overflows for _N = 100000
                     bench "create" $ whnf makePSQ randomNumbers
                   , bench "create + toList" $ nf (toListPS . makePSQ) randomNumbers
                   , bench "toList" $ nf toListPS preparedPSQ
                   , bench "keys" $ nf PSQ.keys preparedPSQ
                   , bench "findMin" $ nf (fmap toTupPS . PSQ.findMin) preparedPSQ
                   , bench "size" $ nf PSQ.size preparedPSQ
                   , bench "top 2" $ nf top2PSQ preparedPSQ
                   , bench "findmin sequential" $ nf findminPSQsequential 10000
                   ]
    , bgroup "FT"  [ bench "create" $ whnf makeFT randomNumbers
                   , bench "create + toList" $ nf (toListFT . makeFT) randomNumbers
                   , bench "toList" $ nf toListFT preparedFT
                   , bench "keys" $ nf FT.keys preparedFT
                   , bench "findMin" $ nf (fmap toTupFT . FT.findMin) preparedFT
                   , bench "size" $ nf FT.size preparedFT
                   , bench "top 2" $ nf top2FT preparedFT
                   , bench "findmin sequential" $ nf findminFTsequential 10000
                   ]
    , bgroup "QL"  [ bench "create" $ whnf makeQL randomNumbers
                   , bench "create + toList" $ nf (toListQL . makeQL) randomNumbers
                   , bench "toList" $ nf toListQL preparedQL
                   -- keys does not exist here
                   , bench "findMin" $ nf (fmap toTupQL . QL.top) preparedQL
                   , bench "size" $ nf QL.size preparedQL
                   , bench "top 2" $ nf top2QL preparedQL
                   , bench "findmin sequential" $ nf findminQLsequential 10000
                   ]
    , bgroup "GHC" [ bench "create" $ whnf (makeGHC ghcUniques) randomNumbers
                   , bench "create + toList" $ nf (toListGHC . makeGHC ghcUniques) randomNumbers
                   , bench "toList" $ nf toListGHC preparedGHC
                   -- keys does not exist here
                   , bench "findMin" $ nf (fmap toTupGHC . GHC.findMin) preparedGHC
                   , bench "size" $ nf GHC.size preparedGHC
                   , bench "top 2" $ nf top2GHC preparedGHC
                   , bench "findmin sequential" $ nf (findminGHCsequential crashCheckUniques) 10000
                   ]
    ]
  where
    -- PSQueue
    makePSQ :: [Int] -> PSQ.PSQ Int Double
    makePSQ randomNumbers = PSQ.fromList [ r PSQ.:-> fromIntegral r*2 | r <- randomNumbers ]

    findminPSQsequential :: Int -> String
    findminPSQsequential n = case PSQ.findMin (PSQ.fromList [ show p PSQ.:-> p | p <- [1..n] ]) of
      Just (k PSQ.:-> p) | p > 0 -> k
      _                          -> error "findminPSQsequential: should not happen"

    toTupPS (k PSQ.:-> p) = (k, p)

    toListPS = map toTupPS . PSQ.toList

    top2PSQ q = (\(m, rest) -> (toTupPS m, toTupPS <$> PSQ.findMin rest)) <$> PSQ.minView q

    -- FingerTree.PSQueue
    makeFT :: [Int] -> FT.PSQ Int Double
    makeFT randomNumbers = FT.fromList [ r FT.:-> (fromIntegral r*2) | r <- randomNumbers ]

    findminFTsequential :: Int -> String
    findminFTsequential n = case FT.findMin (FT.fromList [ show p FT.:-> p | p <- [1..n] ]) of
      Just (k FT.:-> p) | p > 0 -> k
      _                         -> error "findminFTsequential: should not happen"

    toTupFT (k FT.:-> p) = (k, p)

    toListFT = map toTupFT . FT.toList

    top2FT q = (\(m, rest) -> (toTupFT m, toTupFT <$> FT.findMin rest)) <$> FT.minView q

    -- queuelike PQueue
    makeQL :: [Int] -> QL.PQueue (Int :-> Double)
    makeQL randomNumbers = QL.fromList [ r QL.:-> (fromIntegral r*2) | r <- randomNumbers ]

    findminQLsequential :: Int -> String
    findminQLsequential n = case QL.top (QL.fromList [ show p QL.:-> p | p <- [1..n] ] :: QL.PQueue (String :-> Int)) of
      Just (k QL.:-> p) | p > 0 -> k
      _                         -> error "findminQLsequential: should not happen"

    toTupQL (k QL.:-> p) = (k, p)

    toListQL = map toTupQL . QL.toList

    top2QL q = (\(m, rest) -> (toTupQL m, toTupQL <$> QL.top rest)) <$> QL.extract q

    -- GHC.Event.PSQ
    makeGHC :: [Unique] -> [Int] -> GHC.PSQ Int
    makeGHC uniques randomNumbers = GHC.fromList [ GHC.E u (fromIntegral r * 2) r | (u, r) <- zip uniques randomNumbers ]

    findminGHCsequential :: [Unique] -> Int -> String
    findminGHCsequential uniques n = case GHC.findMin (GHC.fromList [ GHC.E u (fromIntegral p) (show p) | (u, p) <- zip uniques [1..n] ]) of
      Just (GHC.E _ p v) | p > 0 -> v
      _                          -> error "findminGHCsequential: should not happen"

    toTupGHC GHC.E { GHC.prio = p, value = r } = (r, p)

    toListGHC = map toTupGHC . GHC.toList

    top2GHC q = (\(m, rest) -> (toTupGHC m, toTupGHC <$> GHC.findMin rest)) <$> GHC.minView q
