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


capTableToStock  capConversion=
  [opengame|
        inputs    : capTable,valuation;
        feedback  : ;

        :----------------------------:

        inputs    : capTable,valuation;
        feedback  : ;
        operation : forwardFunction capConversion;
        outputs   : stockValPair ;
        returns   : ;

        :----------------------------:

        outputs   : stockValPair;
        returns   : ;
    |]
