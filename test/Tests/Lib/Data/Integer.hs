{---------------------------------------------------------------------}
{- Copyright 2015, 2016 Nathan Bloomfield                            -}
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

module Tests.Lib.Data.Integer where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (arbitrary)
import Test.QuickCheck

import Tests.Util
import Tests.Lib.Ring



{----------}
{- :Suite -}
{----------}

testInteger :: TestTree
testInteger = testGroup "Integer"
  [ testRingoid  (0::Integer)
  , testCRingoid (0::Integer)
  , testURingoid (0::Integer)
  , testORingoid (0::Integer)
  , testEDoid    (0::Integer)
  , testGCDoid   (0::Integer)
  ]



{---------------}
{- :Generators -}
{---------------}

instance RingoidArb  Integer
instance CRingoidArb Integer
instance URingoidArb Integer
instance ORingoidArb Integer

instance GCDoidArb Integer where
  rGCDLNeut _ = arbitrary >>= \a -> return (0,a)
  rGCDRNeut _ = arbitrary >>= \a -> return (a,0)

instance EDoidArb Integer where
  rQuotRem _ = do
    a <- arbitrary
    NonZero b <- arbitrary
    return (a,b)


{-
  , testGroup "intSub"
      [ testProperty "a-a == 0                 " $ prop_intSub_zero
  , testGroup "intAbs"
      [ testProperty "|a| == |-a|              " $ prop_intAbs_negative
      ]
  ]

-}