{-# LANGUAGE DataKinds #-}

module Analysis.AuxiliaryFunctions where

import Contracts.ShipmentAndDelivery.Export
import OpenGames.Engine.Engine
import Numeric.Probability.Distribution (decons)

computeExpectation
  :: Stochastic (Payoff, Payoff)
     -> (Payoff, Payoff)
computeExpectation = sumPair . Prelude.map (\((x1,x2),p) -> (x1 * p,x2 * p)) . decons
 where
    sumPair xs = (sum $ fmap fst xs, sum $ fmap snd xs)

