{---------------------------------------------------------------------}
{- Copyright 2016 Nathan Bloomfield                                  -}
{-                                                                   -}
{- This file is part of Carl.                                        -}
{-                                                                   -}
{- Carl is free software: you can redistribute it and/or modify      -}
{- it under the terms of the GNU General Public License version 3,   -}
{- as published by the Free Software Foundation.                     -}
{-                                                                   -}
{- Carl is distributed in the hope that it will be useful, but       -}
{- WITHOUT ANY WARRANTY; without even the implied warranty of        -}
{- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      -}
{- GNU General Public License for more details.                      -}
{-                                                                   -}
{- You should have received a copy of the GNU General Public License -}
{- along with Carl. If not, see <http://www.gnu.org/licenses/>.      -}
{---------------------------------------------------------------------}

module Tests.Lib.Data.GF2 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Carl.Data.GF2

import Tests.Util
import Tests.Lib.Ring



{----------}
{- :Suite -}
{----------}

testGF2 :: TestTree
testGF2 = testGroup "GF2"
  [ testRingoid  (0 :: GF2)
  , testCRingoid (0 :: GF2)
  , testURingoid (0 :: GF2)
  ]



{---------------}
{- :Generators -}
{---------------}

instance Arbitrary GF2 where
  arbitrary = elements [0,1]

instance RingoidArb  GF2
instance CRingoidArb GF2
instance URingoidArb GF2
