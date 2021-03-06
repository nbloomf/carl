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

module Carl.Data.ZZMod where

import Carl.Algebra.Ring
import Carl.Canon
import Carl.Write.LaTeX
import Carl.Write.Unicode

data ZZModulo = ZZModulo
  { residue :: Integer
  , modulus :: Integer
  } deriving Show

zzmod :: Integer -> Integer -> ZZModulo
a `zzmod` 0 = ZZModulo a 0
a `zzmod` n = ZZModulo b m
  where
    m = abs n
    b = ((a `rem` m) + m) `rem` m

instance Canon ZZModulo where
  canon (ZZModulo a n) = zzmod a n

showZZMod :: ZZModulo -> String
showZZMod (ZZModulo a n) = show a ++ " mod " ++ show n

instance Eq ZZModulo where
  (ZZModulo a n) == (ZZModulo b m)
    | n == 0    = m == 0 && a == b
    | otherwise = m == n && ((a-b) `rem` n == 0)

instance Ringoid ZZModulo where
  rAdd (ZZModulo a n) (ZZModulo b m)
    | n /= m = Left DifferentModulus
    | otherwise = Right $ (a+b) `zzmod` n

  rMul (ZZModulo a 0) (ZZModulo b m) = Right $ (a*b) `zzmod` m
  rMul (ZZModulo a n) (ZZModulo b 0) = Right $ (a*b) `zzmod` n
  rMul (ZZModulo a n) (ZZModulo b m)
    | n /= m = Left DifferentModulus
    | otherwise = Right $ (a*b) `zzmod` n

  rNeg (ZZModulo a n) = (-a) `zzmod` n

  rZero = 0 `zzmod` 0

  rIsZero (ZZModulo a 0) = a == 0
  rIsZero (ZZModulo a n) = (a `rem` n) == 0

  rNeutOf (ZZModulo _ n) = Right $ ZZModulo 0 n
  rLAnnOf (ZZModulo _ n) = Right $ ZZModulo 0 n
  rRAnnOf (ZZModulo _ n) = Right $ ZZModulo 0 n


instance CRingoid ZZModulo


instance URingoid ZZModulo where
  rOne = ZZModulo 1 0

  rIsOne (ZZModulo a 0) = a == 1
  rIsOne (ZZModulo a n) = (a `rem` n) == 1

  rIsUnit (ZZModulo a 0) = Right $ a==1 || a==(-1)
  rIsUnit (ZZModulo a n) = Right $ (a `gcd` n) == 1

  rInjInt a = Right (ZZModulo a 0)

  rInv (ZZModulo a n)
    | (a `gcd` n) /= 1 = Left $ RingoidNotInvertibleErr (show a ++ " mod " ++ show n)
    | otherwise = do
        (h,_) <- rBezout a n
        return (h `zzmod` n)

  rLOneOf (ZZModulo _ n) = Right $ ZZModulo 1 n
  rROneOf (ZZModulo _ n) = Right $ ZZModulo 1 n


{- :Write -}

instance Unicode ZZModulo where
  unicode (ZZModulo a n) = show a ++ " mod " ++ show n
  unicodeCompact a = show $ residue a

instance LaTeX ZZModulo where
  latex (ZZModulo a n) = show a ++ "\\ \\mathrm{mod}\\ " ++ show n
  latexCompact a = show $ residue a
