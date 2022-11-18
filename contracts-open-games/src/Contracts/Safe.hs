-- Imports here
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------
module Contracts.Safe where

import Engine.Engine
import Preprocessor.Preprocessor

---------------
-- 0 Data types
-- Players: Investor, Company
-- Actions: cap is set to a certain percentage, then the investor and company can choose to settle or not settle
---------------
type Actions = Int

-- Action space
type ActionSpace = [Action]

-- Acceptance decision
data Acceptance = Accept | Reject
  deriving (Show, Eq, Ord)

------------
-- 1 Payoffs
-- The payoff is the amount of cap table space that the
------------
data SafeMove = Settle | DontSettle deriving (Eq, Ord, Show)