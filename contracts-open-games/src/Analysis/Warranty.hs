{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis.Warranty where

import Contracts.Warranty.Clauses
import Engine.Engine


-- Define scenarios of interest
scenario1 = warrantyCostsNoWarranty "seller" "buyer" 100 0.5

scenario2 = warrantyCostsExAnteLimitedWarranty "seller" "buyer" 100 0.5 0.5


-- Run simulations
-- Only one step as outcome is deterministic
simulatedScenario1 :: Stochastic (Payoff, Payoff)
simulatedScenario1 = nextState (play scenario1  Nil) ()

simulatedScenario2 :: Stochastic (Payoff, Payoff)
simulatedScenario2 = nextState (play scenario2 Nil) ()
