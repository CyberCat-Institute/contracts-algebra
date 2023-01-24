{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Contracts.Safe.ExitDecision where

import Contracts.Safe.Types
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

-- exit or no?
data ExitDecisionMatrix = Exit | Stay deriving (Show, Eq, Ord)

-- function to determine if the SAFE Investor will be backing out of the deal
safeExitDecision :: CapTable -> ExitDecisionMatrix
safeExitDecision = undefined

-- FIXME Is this supposed to be a function or a decision [decision]?
-- FIXME This whole module does not make sense
-- FIXME Is this function supposed to compute the decision; if so why are there then several outputs below?
{-
exitDecision = [opengame|

        inputs    : capTable ;
        feedback  : ;

        :----------------------------:

        inputs    : capTable;
        feedback  : ;
        operation : dependentDecision "Investor" (const [Exit,Stay]);
        outputs   : ExitDecisionMatrix, CashOut ;
        returns   : ;
        // TODO check payoffs

        :----------------------------:

        outputs   : capTableNew, CashOut;
        returns   : ;
    |]
--}
