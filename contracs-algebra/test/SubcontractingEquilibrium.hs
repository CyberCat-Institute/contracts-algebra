{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module SubcontractingEquilibrium where

import           Examples.Contracts.Subcontracting
import           Engine.Engine

import           GHC.Generics
import           Generic.Random
import           Test.QuickCheck




main =
  verboseCheck prop_eqNoSubContracting


------------------------------------------------
-- Explore tests of given strategies relative to
-- random starting conditions, i.e. chains

-- draw only positive numbers
genPosInt :: Gen Int
genPosInt = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

genPosDouble :: Gen Double
genPosDouble = abs `fmap` (arbitrary :: Gen Double) `suchThat` (> 0)

genPosTuple = sequence (genPosDouble,genPosDouble)

-- Equilibrium condition for no subcontracting following fixed strategey 
eqNo minPrice maxPrice parameterProvider parameterSubCon qualityPref providerPrice =
  checkEq minPrice maxPrice parameterProvider parameterSubCon qualityPref providerPrice == True
  where
   checkEq minPrice maxPrice parameterProvider parameterSubCon qualityPref providerPrice =
     generateEquilibrium $
        evaluate (noSubContract "Provider" "Client" minPrice maxPrice [High,Low] parameterProvider qualityPref)
              (stratProviderPrice providerPrice ::- stratProviderHQuality ::- stratClient ::- Nil)
              void

prop_eqNoSubContracting =
  forAll genPosDouble (\x1 ->
       forAll genPosDouble (\x2 ->
          forAll genPosDouble (\x3 ->
             forAll genPosDouble (\x4 ->
                forAll genPosDouble (\x5 ->
                    forAll genPosDouble (\x6 -> eqNo x1 x2 x3 x4 x5 x6 ))))))


