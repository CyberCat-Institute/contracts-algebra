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

equityConversion :: CapTable -> SafeInvestment -> SeriesValuation -> CapTable

-- FIXME Above function does not match with used parameters belo9;32Mw

conversion CapTable =
  [opengame|
        inputs    : investment,valuation,CapTable;
        feedback  : ;

        :----------------------------:

        inputs    : investment,valuation;
        feedback  : ;
        operation : forwardFunction $ uncurry $  equityConversion CapTable investment valuation;
        outputs   : CapTableNew ;
        returns   : ;

        :----------------------------:

        outputs   : CapTableNew;
        returns   : ;
    |]
