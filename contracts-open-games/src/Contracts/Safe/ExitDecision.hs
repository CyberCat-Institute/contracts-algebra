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
import qualified Data.Map.Strict as M
import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import GHC.Base (undefined)

{-
Exit decision
-}
-- function to determine if the SAFE Investor will be backing out of the deal
-- TODO use a lens for the capTable?
safeExitDecision :: String -> CapTable -> ExitDecisionMatrix -> (Double,CapTable)
safeExitDecision  name capTable decision =
  case decision of
    Exit -> (value, M.delete name capTable)
    Stay -> (0, capTable)
  where
    value = capTable M.! name


multiply = (*)

exitDecision name = [opengame|

        inputs    : capTableOld, valuation ;
        feedback  : ;

        :----------------------------:

        inputs    : capTableOld ;
        feedback  : ;
        operation : dependentDecision name (const [Exit,Stay]);
        outputs   : exitDecision ;
        returns   : value ;
        // TODO check payoffs

        inputs    : capTableOld, exitDecision ;
        feedback  : ;
        operation : forwardFunction $ uncurry $ safeExitDecision name ;
        outputs   : (share,capTableNew) ;
        returns   :  ;

        inputs    : share, valuation ;
        feedback  : ;
        operation : forwardFunction $ uncurry multiply ;
        outputs   : value ;
        returns   :  ;



        :----------------------------:

        outputs   : capTableNew ;
        returns   : ;
    |]
