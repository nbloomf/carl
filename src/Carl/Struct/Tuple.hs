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

module Carl.Struct.Tuple (
  Tuple(), arity, project
) where

import Carl.AlgErr

import Data.List (genericLength)

data Tuple a = Tuple [a]
  deriving (Eq, Show)

arity :: Tuple a -> Integer
arity (Tuple es) = genericLength es

project :: Tuple a -> Integer -> Either AlgErr a
project (Tuple es) k = undefined
