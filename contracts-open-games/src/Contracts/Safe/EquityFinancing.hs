{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Contracts.Safe.EquityFinancing where

import Contracts.Safe.Types
import OpenGames.Engine.Engine
import OpenGames.Examples.Auctions.AuctionSupportFunctions
import OpenGames.Preprocessor

-- data SAFEReinvest = Reinvest | DontReinvest deriving (Show, Eq, Ord)

-- pricedRound :: HowMuchToRaise -> z -> SeriesInvestment -> SeriesValuation -> Reinvest


-- Draws a value and creates a pair of _value_ _name_
natureDrawsTypeStage name = [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    :   ;
    feedback  :   ;
    operation : nature (uniformDist values) ;
    outputs   : value ;
    returns   :  ;
    :-----:

    outputs   :  (name,value) ;
    returns   :    ;
  |]
-- Individual bidding stage
investorBid name = [opengame|

    inputs    :  nameValuePair  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  nameValuePair  ;
    feedback  :   ;
    operation :  dependentDecision name (const [0,1000..9999]) ;
    outputs   :  dec ;
    returns   :  setPayoff nameValuePair payments  ;
    :---------------------------:

    outputs   :  dec ;
    returns   :  payments  ;
  |]

  -- Transforms the payments into a random reshuffling
transformPayments kPrice kSlots noLotteries paymentFunction = [opengame|

   inputs    : bids ;
   feedback  :      ;

   :-----------------:
   inputs    : bids ;
   feedback  :      ;
   operation : liftStochasticForward shuffleBids ;
   outputs   : shuffledBids ;
   returns   :      ;

   inputs    : shuffledBids ;
   feedback  :      ;
   operation : forwardFunction (auctionPayment paymentFunction reservePrice kPrice kSlots noLotteries) ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]

  bidding kPrice kSlots noLotteries paymentFunction = [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "VC1" ;
   outputs   :  VC1Value ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "VC2" ;
   outputs   :  VC2Value ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "VC3" ;
   outputs   :  VC3Value ;
   returns   :      ;

   inputs    :  VC1Value    ;
   feedback  :      ;
   operation :  investorBid "VC1" ;
   outputs   :  VC1Dec ;
   returns   :  payments  ;

   inputs    :  VC2Value    ;
   feedback  :      ;
   operation :  investorBid "VC2" ;
   outputs   :  VC2Dec ;
   returns   :  payments  ;

   inputs    :  VC3Value    ;
   feedback  :      ;
   operation :  investorBid "VC3" ;
   outputs   :  VC3Dec ;
   returns   :  payments  ;

   inputs    :  [("VC1",VC1Dec),("VC2",VC2Dec),("VC3",VC3Dec)]  ;
   feedback  :      ;
   operation :   transformPayments kPrice kSlots noLotteries paymentFunction ;
   outputs   :  payments ;
   returns   :      ;
   :-----------------:

   outputs   :      ;
   returns   :      ;
   |]

equityFinancing name howMuchToRaise valuation payments=
  [opengame|
        inputs    : howMuchToRaise,valuation,payments ;
        feedback  : ;

        :----------------------------:
        inputs    :      ;
        feedback  :      ;
        operation : dependentDecision name (const [Reinvest, DontReinvest]);
        outputs   : SAFEReinvest;
        returns   : ;
        // Adds SAFE investor to the new valuation if they decide to reinvest. Since they're already investors, they don't participate in the auction
        inputs    : howMuchToRaise,valuation,SAFEReinvest,payments;
        feedback  : ;
        operation : //TODO: create a function that uses the auction and post-money valuation to return a CapTable and a valuation;
        outputs   : capTable, seriesValuation;
        returns   : exitPayoff;

        :----------------------------:

        outputs   : capTable, seriesValuation;
        returns   : exitPayoff;
    |]
