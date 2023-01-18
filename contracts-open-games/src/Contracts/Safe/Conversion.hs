{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}


module Contracts.Safe.Conversion where

import Contracts.Safe.Types
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

-- equityConversion :: SafeInvestment -> SeriesValuation -> StockValPair -> CapTable

conversion equityConversion =
  [opengame|
        inputs    : investment,valuation;
        feedback  : ;

        :----------------------------:

        inputs    : investment,valuation;
        feedback  : ;
        operation : forwardFunction equityConversion;
        outputs   : (investor,ownershipPercentage) ;
        returns   : ;

        :----------------------------:

        outputs   : (investor,ownershipPercentage);
        returns   : ;
    |]
