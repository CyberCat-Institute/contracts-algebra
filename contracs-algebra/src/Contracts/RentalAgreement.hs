{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.RentalAgreement where

--import Engine.Engine
--import Preprocessor.Preprocessor


payoffLandLord:: Double -> Double
payoffLandLord rent = rent

payoffTenant :: Double -> Double
payoffTenant rent = rent


actionSpaceLandLord= [0, 1..10]

rentGame actionSpaceLandLord = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:
    inputs    : ;
    feedback  : ;
    operation : dependentDecision "landLord" (const actionSpaceLandLord) ;
    outputs   : rent ;
    returns   : payoffLandLord rent ;
    :-----:

    outputs   : rent  ;
    returns   :  ;
|]

-- TODO: Time based operations like late fees require control flow which is awaiting operator
--       What is the tenant utility?
