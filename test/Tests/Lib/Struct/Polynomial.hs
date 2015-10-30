{---------------------------------------------------------------------}
{- Copyright 2015 Nathan Bloomfield                                  -}
{-                                                                   -}
{- This file is part of Feivel.                                      -}
{-                                                                   -}
{- Feivel is free software: you can redistribute it and/or modify    -}
{- it under the terms of the GNU General Public License version 3,   -}
{- as published by the Free Software Foundation.                     -}
{-                                                                   -}
{- Feivel is distributed in the hope that it will be useful, but     -}
{- WITHOUT ANY WARRANTY; without even the implied warranty of        -}
{- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      -}
{- GNU General Public License for more details.                      -}
{-                                                                   -}
{- You should have received a copy of the GNU General Public License -}
{- along with Feivel. If not, see <http://www.gnu.org/licenses/>.    -}
{---------------------------------------------------------------------}

{-# OPTIONS_GHC -XTypeSynonymInstances #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module Tests.Lib.Struct.Polynomial where

{-------------------}
{- Contents        -}
{-   :Suites       -}
{-   :Generators   -}
{-   :Properties   -}
{-------------------}

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Carl.Struct.Polynomial
import Carl.Algebra.Ring
import Carl.Data.Monomial
import Carl.Data.Natural
import Carl.Canon

import Tests.Util
import Tests.Lib.Ring


{-----------}
{- :Suites -}
{-----------}

testRingoidPoly :: (RingoidArb t, CRingoidArb t, Show t, Canon t, Eq t) => t -> TestTree
testRingoidPoly t = testRingoid (constPoly t)

testCRingoidPoly :: (RingoidArb t, CRingoidArb t, Show t, Canon t, Eq t) => t -> TestTree
testCRingoidPoly t = testCRingoid (constPoly t)

testEDoidPoly :: (RingoidArb t, CRingoidArb t, URingoidArb t, Canon t, Show t, Eq t) => t -> TestTree
testEDoidPoly t = testEDoid (constPoly t)


{---------------}
{- :Generators -}
{---------------}

g_MAX_NUM_TERMS :: Int
g_MAX_NUM_TERMS = 5

varchar :: [Char]
varchar =
  "abcdefghijklmnopqrstuvwxyz" ++
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
  "_[]()"

var :: Gen String
var = listOf1 $ elements varchar

arbPowerOf :: Variable -> Gen (Monomial Variable)
arbPowerOf x = do
  k <- arbitrary
  return $ makeMonomial [(x, k)]

instance Arbitrary Natural where
  arbitrary = do
    NonNegative k <- arbitrary
    return (Nat k)

instance Arbitrary Variable where
  arbitrary = do
    cs <- var
    return (Var cs)

instance Arbitrary (Monomial Variable) where
  arbitrary = do
    t  <- choose (1,5)
    xs <- vectorOf t arbitrary
    ks <- vectorOf t arbitrary
    return $ canon $ makeMonomial $ zip xs ks

arbRingoidPoly :: (RingoidArb t) => t -> Gen (Poly t)
arbRingoidPoly x = do
  t  <- choose (1, g_MAX_NUM_TERMS)
  cs <- rLocalElts x t
  xs <- vectorOf t arbitrary
  return $ fromTerms $ zip cs xs

instance (Arbitrary a, RingoidArb a) => Arbitrary (Poly a) where
  arbitrary = do
    x  <- arbitrary
    arbRingoidPoly x

instance (RingoidArb a, CRingoidArb a, Canon a, Eq a) => RingoidArb (Poly a) where
  rAddAssoc _ = do
    x  <- arbitrary
    p1 <- arbRingoidPoly x
    p2 <- arbRingoidPoly x
    p3 <- arbRingoidPoly x
    return (p1, p2, p3)

  rAddComm _ = do
    x  <- arbitrary
    p1 <- arbRingoidPoly x
    p2 <- arbRingoidPoly x
    return (p1, p2)

  rMulAssoc _ = do
    x  <- arbitrary
    p1 <- arbRingoidPoly x
    p2 <- arbRingoidPoly x
    p3 <- arbRingoidPoly x
    return (p1, p2, p3)

  rMulDistLrAdd _ = do
    x  <- arbitrary
    p1 <- arbRingoidPoly x
    p2 <- arbRingoidPoly x
    p3 <- arbRingoidPoly x
    return (p1, p2, p3)

  rMulDistRrAdd _ = do
    x  <- arbitrary
    p1 <- arbRingoidPoly x
    p2 <- arbRingoidPoly x
    p3 <- arbRingoidPoly x
    return (p1, p2, p3)


instance (RingoidArb a, CRingoidArb a) => CRingoidArb (Poly a) where
  rMulComm _ = do
    x  <- arbitrary
    p1 <- arbRingoidPoly x
    p2 <- arbRingoidPoly x
    return (p1, p2)

instance (RingoidArb a, CRingoidArb a, URingoidArb a, Canon a, Eq a) => EDoidArb (Poly a) where
  rQuotRem _ = do
    t  <- choose (1, g_MAX_NUM_TERMS)
    as <- vectorOf t arbitrary
    let Right a = fromCoefficients (variable $ Var "x") as
    u  <- choose (1, g_MAX_NUM_TERMS)
    cs <- vectorOf u arbitrary
    let bs = if all rIsZero cs
               then cs ++ [rOne]
               else cs
    let Right b = fromCoefficients (variable $ Var "x") bs
    return (a,b)