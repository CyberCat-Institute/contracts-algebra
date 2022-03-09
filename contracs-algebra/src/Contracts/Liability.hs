{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.Liability where


-- Liability clauses

-- TODO isolate clause elements


---------------
-- 0 Data types
---------------

-- Actions are just some index numbers?
type Action = Int

-- Action space
type ActionSpace = [Action]

-- What is the possible damage that can result
type Damage = Double

-- What liability regime do we pursue?
-- FIXME Both is here also interpreted as a default
-- data Liability a = Range a | Default -- for instance last ruling of courts
data Liability = Player1 | Player2 | Both
  deriving (Show,Eq,Ord)

-- Acceptance decision
data Acceptance = Accept | Reject
  deriving (Show,Eq,Ord)


------------
-- 1 Payoffs
------------
-- One example of actions taken
costlyActions :: Int -> Double
costlyActions = (* (-0.2)) . fromIntegral

-- Distribute damage
distributeDamage Player1 damage = (- damage,0)
distributeDamage Player2 damage = (0, - damage)
distributeDamage Both    damage = (d2,d2)
  where d2 = - damage / 2

------------------------
-- 2 Auxiliary functions
------------------------

-- Stochastic process which determines the amount of damage; both players affect the damage
-- Both players being active is more effective than a single player
damageFunctionBoth :: [Action] -> Damage -> Damage -> Action -> Action -> Stochastic Damage
damageFunctionBoth space lowDamage highDamage a1 a2 = do
  let agg = fromIntegral $ (a1 + a2)
      p = agg / (fromIntegral $ 2 *  maximum space)
      q = 1 - p
  distFromList [(lowDamage,p),(highDamage,q)]

-- Stochastic process which determines the amount of damage; only first player
damageFunction1 :: [Action] -> Damage -> Damage -> Action -> Action -> Stochastic Damage
damageFunction1 space lowDamage highDamage a1 _ = do
  let agg = fromIntegral $ a1
      p = agg / (fromIntegral $ maximum space)
      q = 1 - p
  distFromList [(lowDamage,p),(highDamage,q)]

-- Stochastic process which determines the amount of damage; only first player
damageFunction2 :: [Action] -> Damage -> Damage -> Action -> Action -> Stochastic Damage
damageFunction2 space lowDamage highDamage _ a2= do
  let agg = fromIntegral $ a2
      p = agg / (fromIntegral $ maximum space)
      q = 1 - p
  distFromList [(lowDamage,p),(highDamage,q)]


-- Determine decision on liability regime given proposal and acceptance decision by player 2
-- If no agreement, default to both share liabilities
determineRegime :: Liability -> Acceptance -> Liability
determineRegime x Accept = x
determineRegime _ Reject = Both


--------------------
-- 3 Building Blocks
--------------------
-- How do the actions of two agents lead to damage?
-- "Production function" of damage
actionTaken  actionSpace payoffFunction1 payoffFunction2 transformAction lowDamage highDamage = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:
    label     : effortP1 ;
    inputs    : ;
    feedback  : ;
    operation : dependentDecision "player1" (const actionSpace) ;
    outputs   : action1 ;
    returns   : payoffFunction1 action1;

    label     : effortP2 ;
    inputs    : ;
    feedback  : ;
    operation : dependentDecision "player2" (const actionSpace) ;
    outputs   : action2 ;
    returns   : payoffFunction2 action2;

    label     : damageFunction ;
    inputs    : action1, action2 ;
    feedback  : ;
    operation : liftStochasticForward $ uncurry $ transformAction actionSpace lowDamage highDamage  ;
    outputs   : damage ;
    returns   : ;


    :-----:

    outputs   : damage ;
    returns   :  ;
|]

-- Given liability agreement, fix costs for players accordingly
liabilitySettlement  = [opengame|

    inputs    : regime, damage ;
    feedback  : ;

    :-----:
    label     : distributeDamage ;
    inputs    : regime, damage ;
    feedback  : ;
    operation : forwardFunction $ uncurry distributeDamage;
    outputs   : (damage1,damage2) ;
    returns   : ;

    label     : damageForP1 ;
    inputs    : damage1;
    feedback  : ;
    operation :  addPayoffs "player1" ;
    outputs   : ;
    returns   : ;

    label     : damageForP2 ;
    inputs    : damage2;
    feedback  : ;
    operation : addPayoffs "player2" ;
    outputs   : ;
    returns   : ;


    :-----:

    outputs   :  ;
    returns   :  ;
|]


-- Determine liability regime
liabilityAgreement = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    label     : player1Agreement ;
    inputs    : ;
    feedback  : ;
    operation : dependentDecision "player1" (const [Player1,Player2,Both]) ;
    outputs   : proposal ;
    returns   : 0;

    label     : player2Agreement ;
    inputs    : proposal;
    feedback  : ;
    operation : dependentDecision "player2" (const [Accept,Reject]) ;
    outputs   : dec2 ;
    returns   : 0;


    label     : determineRegime ;
    inputs    : proposal, dec2 ;
    feedback  : ;
    operation : forwardFunction $ uncurry determineRegime ;
    outputs   : regime ;
    returns   : ;


    :-----:

    outputs   : regime ;
    returns   :  ;
|]


------------------
-- 4 Complete Game
------------------

-- Given liability agreement, fix costs for players accordingly
completeGame :: [Action]
             -> (Action -> Double)
             -> (Action -> Double)
             -> ([Action] -> Damage -> Damage -> Action -> Action -> Stochastic Double)
             -> Damage
             -> Damage
             -> OpenGame
                   StochasticStatefulOptic
                   StochasticStatefulContext
                   '[Kleisli Stochastic () Liability,
                     Kleisli Stochastic Liability Acceptance, Kleisli Stochastic () Action,
                     Kleisli Stochastic () Action]
                   '[[DiagnosticInfoBayesian () Liability],
                     [DiagnosticInfoBayesian Liability Acceptance],
                     [DiagnosticInfoBayesian () Action], [DiagnosticInfoBayesian () Action]]
                   ()
                   ()
                   ()
                   ()
completeGame actionSpace payoffFunction1 payoffFunction2 transformAction lowDamage highDamage = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    label     : libalityAgreement ;
    inputs    : ;
    feedback  : ;
    operation : liabilityAgreement ;
    outputs   : regime ;
    returns   : ;

    label     : actionTaken ;
    inputs    : ;
    feedback  : ;
    operation : actionTaken actionSpace payoffFunction1 payoffFunction2 transformAction lowDamage highDamage;
    outputs   : damage ;
    returns   : ;

    label     : damageSettlement ;
    inputs    : regime, damage;
    feedback  : ;
    operation : liabilitySettlement  ;
    outputs   : ;
    returns   : ;


    :-----:

    outputs   : ;
    returns   : ;
|]



completeGameExogenousRegime regime actionSpace payoffFunction1 payoffFunction2 transformAction lowDamage highDamage = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    label     : actionTaken ;
    inputs    : ;
    feedback  : ;
    operation : actionTaken actionSpace payoffFunction1 payoffFunction2 transformAction lowDamage highDamage;
    outputs   : damage ;
    returns   : ;

    label     : damageSettlement ;
    inputs    : regime, damage;
    feedback  : ;
    operation : liabilitySettlement  ;
    outputs   : ;
    returns   : ;


    :-----:

    outputs   : ;
    returns   : ;
|]




eqCompleteGame actionSpace payoffFunction1 payoffFunction2 transformAction lowDamage highDamage strat =
  generateIsEq $
    evaluate
      (completeGame actionSpace payoffFunction1 payoffFunction2 transformAction lowDamage highDamage)
      strat
      void

outputCompleteGame actionSpace payoffFunction1 payoffFunction2 transformAction lowDamage highDamage strat =
  generateOutput $
    evaluate
      (completeGame actionSpace payoffFunction1 payoffFunction2 transformAction lowDamage highDamage)
      strat
      void

eqCompleteGameExogenousRegime regime actionSpace payoffFunction1 payoffFunction2 transformAction lowDamage highDamage strat =
  generateIsEq $
    evaluate
      (completeGameExogenousRegime regime actionSpace payoffFunction1 payoffFunction2 transformAction lowDamage highDamage)
      strat
      void

eqBoth strat =  eqCompleteGame [0,1,2,3,4] costlyActions costlyActions damageFunctionBoth 0 10 strat
eqP1 strat =  eqCompleteGame [0,1,2,3,4] costlyActions costlyActions damageFunction1 0 10 strat
eqP2 strat =  eqCompleteGame [0,1,2,3,4] costlyActions costlyActions damageFunction2 0 10 strat

outputBoth strat =  outputCompleteGame [0,1,2,3,4] costlyActions costlyActions damageFunctionBoth 0 10 strat
outputP1 strat =  outputCompleteGame [0,1,2,3,4] costlyActions costlyActions damageFunction1 0 10 strat
outputP2 strat =  outputCompleteGame [0,1,2,3,4] costlyActions costlyActions damageFunction2 0 10 strat



-----------
-- Strategy

stratAgreement :: Liability -> Kleisli Stochastic () Liability
stratAgreement x = pureAction x

stratAcceptance :: Acceptance ->  Kleisli Stochastic Liability Acceptance
stratAcceptance x = Kleisli (const $ pure x)

stratAction :: Action -> Kleisli Stochastic () Action
stratAction x = pureAction x

{--
-- player 1 is liable and exerts maximum effort
eqP1 (stratAgreement Player1 ::- stratAcceptance Accept ::- stratAction 4 ::- stratAction 0 ::- Nil)
>>>>>>>> Output
----Analytics begin----
 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:
----Analytics end----
<<<<<<<<<<<<<<<<

-- player 2 is liable and exerts maximum effort
eqP2 (stratAgreement Player2 ::- stratAcceptance Accept ::- stratAction 0 ::- stratAction 4 ::- Nil)
>>>>>>>> Output
----Analytics begin----
 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:
----Analytics end----
<<<<<<<<<<<<<<<<

-- Both sharing liability if both share the damage is an equilibrium but both reduce effort to minimum
eqBoth (stratAgreement Both ::- stratAcceptance Accept ::- stratAction 4 ::- stratAction 4 ::- Nil)
----Analytics begin----
 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:
----Analytics end----
<<<<<<<<<<<<<<<<

NOTE: Payoffs are different across different regimes; i.e. distributional effects

outputP1 (stratAgreement Player1 ::- stratAcceptance Accept ::- stratAction 4 ::- stratAction 0 ::- Nil)
player 1: -0.8
player 2: 0

outputP2 (stratAgreement Player2 ::- stratAcceptance Accept ::- stratAction 1 ::- stratAction 0 ::- Nil)
player 1: 0
player 2: -0.8

outputBoth (stratAgreement Both ::- stratAcceptance Accept ::- stratAction 4 ::- stratAction 4 ::- Nil)
player 1: -0.8
player 2: -0.8

-}
