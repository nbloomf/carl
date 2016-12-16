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

module Carl.Data.GF2 (
  GF2()
) where

import Carl.Algebra.Ring
import Carl.AlgErr ()
import Carl.Canon
import Carl.Write.LaTeX

{----------}
{- :Types -}
{----------}

data GF2 = O | I
  deriving Eq

instance Num GF2 where
  fromInteger 0 = O
  fromInteger 1 = I
  fromInteger _ = undefined

  O + x = x
  x + O = x
  I + I = O

  O * _ = O
  _ * O = O
  I * I = I

  abs = undefined
  signum = undefined

instance Show GF2 where
  show O = "0"
  show I = "1"

instance Canon GF2 where
  canon = id

instance LaTeX GF2 where
  latex = show

{---------------}
{- :Arithmetic -}
{---------------}

instance Ringoid GF2 where
  rAdd a b = return $ a + b
  rMul a b = return $ a * b
  rNeg = id

  rNeutOf _ = return O
  rLAnnOf _ = return O
  rRAnnOf _ = return O

  rZero = O
  rIsZero = (== O)

instance CRingoid GF2 where

instance URingoid GF2 where
  rOne = I
  rIsOne = (== I)

  rIsUnit O = return False
  rIsUnit I = return True

  rLOneOf _ = return I
  rROneOf _ = return I

  rInjInt k = return $ fromInteger $ mod k 2

  rInv O = Left (RingoidDivideByZeroErr $ show O)
  rInv I = return I
