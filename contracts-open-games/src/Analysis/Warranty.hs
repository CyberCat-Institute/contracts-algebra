{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis.Warranty where

import Analysis.AuxiliaryFunctions
import Contracts.Warranty.Clauses
import Engine.Engine


---------------
-- 0 Warranties
---------------

-- Define scenarios of interest
scenarioWarranty1 = warrantyCostsNoWarranty "seller" "buyer" 100 0.5

scenarioWarranty2 = warrantyCostsExAnteLimitedWarranty "seller" "buyer" 100 0.5 0.5


-- Run simulations
-- Only one step as outcome is deterministic
simulatedScenarioWarranty1 :: (Payoff, Payoff)
simulatedScenarioWarranty1 = computeExpectation $ nextState (play scenarioWarranty1  Nil) ()

simulatedScenarioWarranty2 :: (Payoff, Payoff)
simulatedScenarioWarranty2 = computeExpectation $ nextState (play scenarioWarranty2 Nil) ()

