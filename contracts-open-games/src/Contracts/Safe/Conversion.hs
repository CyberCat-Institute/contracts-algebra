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

equityConversion :: SafeInvestment -> SeriesValuation -> StockValPair -> CapTable
equityConversion investment valuation stockValuePair = undefined
-- FIXME Above function does not match with used parameters belo9;32Mw

conversion undefinedParameter  =
  [opengame|
        inputs    : investment,valuation;
        feedback  : ;

        :----------------------------:

        inputs    : investment,valuation;
        feedback  : ;
        operation : forwardFunction $ uncurry $  equityConversion undefinedParameter;
        outputs   : capTable ;
        returns   : ;

        :----------------------------:

        outputs   : capTable;
        returns   : ;
    |]
