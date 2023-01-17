module Contracts.Safe.Types where

import Engine.Engine

---------------
-- Data types
-- Players: Investor, Company
---------------

data SafeMove = Settle | DontSettle deriving (Eq, Ord, Show)

---------------
-- Stochastic variables
-- y: financial performance (valuation) at time 0
-- z: financial performance (valuation) at time 1
---------------
y :: Stochastic Double
y = distFromList [(0.0, 0.5), (100000.0, 0.2), (1000000.0, 0.2), (10000000.0, 0.008), (100000000.0, 0.0019999), (1000000000.0, 0.0000001)]

z :: Stochastic Double
z = distFromList [(0.0, 0.25), (100000.0, 0.25), (1000000.0, 0.25), (10000000.0, 0.1), (100000000.0, 0.1), (1000000000.0, 0.05)]

type SafeInvestment = Int

type HowMuchToRaise = Int

type SeriesInvestment = Int

type SeriesValuation = Int

-- StockValPair = (number of shares, value of share)
type StockValPair = (Int, Int)

-- CapTable = [(company name, % ownership of shares)]
type CapTable = [(String, Int)]

type CashOut = Int