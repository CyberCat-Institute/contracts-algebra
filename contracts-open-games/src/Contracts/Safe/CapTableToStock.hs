{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Contracts.Safe.CapTableToStock where

import Contracts.Safe.Types
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

capConversion :: _

capTableToStock CapTable valuation =
  [opengame|
        inputs    : CapTable,valuation;
        feedback  : ;

        :----------------------------:

        inputs    : investment,valuation;
        feedback  : ;
        operation : forwardFunction capConversion;
        outputs   : StockValPair ;
        returns   : ;

        :----------------------------:

        outputs   : StockValPair;
        returns   : ;
    |]
