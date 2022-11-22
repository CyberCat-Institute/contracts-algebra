{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis.ShipmentAndDelivery where

import Analysis.AuxiliaryFunctions
import Contracts.ShipmentAndDelivery.Export
import Engine.Engine


---------------------------------
-- 0 ShipmentCostsShipperLocation
---------------------------------

-- Define scenarios of interest
scenarioShipmentCostsShipperLocation1 = shipmentCostsShipperLocation "seller" "buyer" 100
scenarioShipmentCostsShipperLocation2 = shipmentCostsPurchaserLocation "seller" "buyer" 100


-- Run simulations
simulatedScenarioShipmentCostsShipperLocation1 :: (Payoff, Payoff)
simulatedScenarioShipmentCostsShipperLocation1 = computeExpectation $ nextState (play scenarioShipmentCostsShipperLocation1  Nil) ()

simulatedScenarioShipmentCostsShipperLocation2 :: (Payoff, Payoff)
simulatedScenarioShipmentCostsShipperLocation2 = computeExpectation $ nextState (play scenarioShipmentCostsShipperLocation2 Nil) ()


-----------------
-- 1 Risk of Loss
-----------------

-- Define scenarios of interest
scenarioRiskOfLoss1 = riskOfLoss1 "seller" "buyer" 100
scenarioRiskOfLoss2 = riskOfLoss2 "seller" "buyer" 100
scenarioRiskOfLoss3 = riskOfLoss3 "seller" "buyer" 100
scenarioRiskOfLoss4 = riskOfLoss4 "seller" "buyer" 100
scenarioRiskOfLoss5 = riskOfLoss5 "seller" "buyer" 100
scenarioRiskOfLoss6 = riskOfLoss6 "seller" "buyer" 100
scenarioRiskOfLoss7 = riskOfLoss7 "seller" "buyer" 100
scenarioRiskOfLoss8 = riskOfLoss8 "seller" "buyer" 100

-- Ex ante scenarios
scenarioRiskOfLoss9 = riskOfLossExpectationBuyer "seller" "buyer" 100 0.5
scenarioRiskOfLoss10 = riskOfLossExpectationSeller "seller" "buyer" 100 0.5

-- Run simulations
simulatedRiskOfLoss1 = computeExpectation $ nextState (play scenarioRiskOfLoss1  Nil) ()
simulatedRiskOfLoss2 = computeExpectation $ nextState (play scenarioRiskOfLoss2  Nil) ()
simulatedRiskOfLoss3 = computeExpectation $ nextState (play scenarioRiskOfLoss3  Nil) ()
simulatedRiskOfLoss4 = computeExpectation $ nextState (play scenarioRiskOfLoss4  Nil) ()
simulatedRiskOfLoss5 = computeExpectation $ nextState (play scenarioRiskOfLoss5  Nil) ()
simulatedRiskOfLoss6 = computeExpectation $ nextState (play scenarioRiskOfLoss6  Nil) ()
simulatedRiskOfLoss7 = computeExpectation $ nextState (play scenarioRiskOfLoss7  Nil) ()
simulatedRiskOfLoss8 = computeExpectation $ nextState (play scenarioRiskOfLoss8  Nil) ()
simulatedRiskOfLoss9 = computeExpectation $ nextState (play scenarioRiskOfLoss9  Nil) ()
simulatedRiskOfLoss10 = computeExpectation $ nextState (play scenarioRiskOfLoss10  Nil) ()

---------------------------
-- 2 Packaging and Shipping
---------------------------

-- Define scenarios of interest
scenarioShipmentPackaging1 = shipmentPackagingCostsBuyerFavorable "seller" "buyer" 100 50 100 50
scenarioShipmentPackaging2 = shipmentPackagingCostsNeutral "seller" "buyer" 100 50 100 50
scenarioShipmentPackaging3 = shipmentPackagingCostsSellerFavorable "seller" "buyer" 100 50 100 50
scenarioShipmentPackaging4 = shipmentPackagingCostsSellerVeryFavorable "seller" "buyer" 100 50 100 50

-- Run simulations
simulatedShipmentPackaging1 = computeExpectation $ nextState (play scenarioShipmentPackaging1 Nil) ()
simulatedShipmentPackaging2 = computeExpectation $ nextState (play scenarioShipmentPackaging2 Nil) ()
simulatedShipmentPackaging3 = computeExpectation $ nextState (play scenarioShipmentPackaging3 Nil) ()
simulatedShipmentPackaging4 = computeExpectation $ nextState (play scenarioShipmentPackaging4 Nil) ()
