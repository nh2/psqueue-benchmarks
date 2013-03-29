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

  defaultMain
    [ bgroup "PSQ" [ -- PSQueue stack overflows for _N = 100000
                     bench "create" $ whnf makePSQ randomNumbers
                   , bench "create + toList" $ nf (toListPS . makePSQ) randomNumbers
                   , bench "toList" $ nf toListPS preparedPSQ
                   , bench "keys" $ nf PSQ.keys preparedPSQ
                   , bench "findMin" $ nf (unMaybeBindingPS . PSQ.findMin) preparedPSQ
                   ]
     , bgroup "FT" [ bench "create" $ whnf makeFT randomNumbers
                   , bench "create + toList" $ nf (toListFT . makeFT) randomNumbers
                   , bench "toList" $ nf toListFT preparedFT
                   , bench "keys" $ nf FT.keys preparedFT
                   , bench "findMin" $ nf (unMaybeBindingFT . FT.findMin) preparedFT
                   ]
     , bgroup "QL" [ bench "create" $ whnf makeQL randomNumbers
                   , bench "create + toList" $ nf (toListQL . makeQL) randomNumbers
                   , bench "toList" $ nf toListQL preparedQL
                   -- keys does not exist here
                   , bench "findMin" $ nf (unMaybeBindingQL . QL.top) preparedQL
                   ]
     , bgroup "GHC" [ bench "create" $ whnf (makeGHC ghcUniques) randomNumbers
                    , bench "create + toList" $ nf (toListGHC . makeGHC ghcUniques) randomNumbers
                    , bench "toList" $ nf toListGHC preparedGHC
                    -- keys does not exist here
                    , bench "findMin" $ nf (unMaybeElemGHC . GHC.findMin) preparedGHC
                    ]
     ]
  where
    -- PSQueue
    makePSQ :: [Int] -> PSQ.PSQ Int Double
    makePSQ randomNumbers = PSQ.fromList [ r PSQ.:-> fromIntegral r*2 | r <- randomNumbers ]

    toListPS psq = [ (r, p) | r PSQ.:-> p <- PSQ.toList psq ]

    unMaybeBindingPS (Just (k PSQ.:-> p)) = Just (k, p)
    unMaybeBindingPS Nothing              = Nothing

    -- FingerTree.PSQueue
    makeFT :: [Int] -> FT.PSQ Int Double
    makeFT randomNumbers = FT.fromList [ r FT.:-> (fromIntegral r*2) | r <- randomNumbers ]

    toListFT ft = [ (r, p) | r FT.:-> p <- FT.toList ft ]

    unMaybeBindingFT (Just (k FT.:-> p)) = Just (k, p)
    unMaybeBindingFT Nothing             = Nothing

    -- queuelike PQueue
    makeQL :: [Int] -> QL.PQueue (Int :-> Double)
    makeQL randomNumbers = QL.fromList [ r QL.:-> (fromIntegral r*2) | r <- randomNumbers ]

    toListQL ql = [ (r, p) | r QL.:-> p <- QL.toList ql ]

    unMaybeBindingQL (Just (k QL.:-> p)) = Just (k, p)
    unMaybeBindingQL Nothing             = Nothing

    -- GHC.Event.PSQ
    makeGHC :: [Unique] -> [Int] -> GHC.PSQ Int
    makeGHC uniques randomNumbers = GHC.fromList [ GHC.E u (fromIntegral r * 2) r | (u, r) <- zip uniques randomNumbers ]

    toListGHC ghc = [ (r, p) | GHC.E { GHC.prio = p , value = r } <- GHC.toList ghc ]

    unMaybeElemGHC (Just (GHC.E { GHC.prio = p , value = r })) = Just (r, p)
    unMaybeElemGHC Nothing                                     = Nothing
