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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Carl.Struct.Polynomial (
  Poly(), Variable(Var), VarString(..), getCoefficients, fromTerm, fromTerms,

  mapCoef, polySeq, coefficientOf,

  coefficients, leadingDegreeBy, isRootOf, monomials,

  typeFixPoly, constPoly, zeroPoly, fromRoots, fromCoefficients, varPoly, varsPoly,

  sumPoly, contentPoly,

  evalPolyAtPolys, evalPolyAtPoly, evalPolyAtScalar,

  showStrP, showP, showUP, showOUP
) where

import qualified Data.Map as M
import Data.List (intersperse, sortBy, maximumBy, nub)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM, join)

import Carl.Algebra.Ring
import Carl.Either ()
import Carl.Pair
import Carl.Canon
import Carl.Monad
import Carl.AlgErr ()
import Carl.Write.LaTeX

import Carl.Data.Natural
import Carl.Data.Monomial

{-------------------}
{- :Contents       -}
{-   :Monadish     -}
{-   :Constructors -}
{-   :Querying     -}
{-   :Arithmetic   -}
{-   :Evaluation   -}
{-   :Instances    -}
{-------------------}



data Variable a = Var
  { unVar :: a
  } deriving (Eq, Ord)

newtype VarString = VarString
  { unVarString :: String
  } deriving (Eq, Ord)

instance Show VarString where
  show = unVarString

instance (Show a) => Show (Variable a) where
  show = show . unVar


newtype Poly x a = Poly
  { unPoly :: M.Map (Monomial (Variable x)) a
  } deriving (Eq, Show)

toTerms :: Poly x a -> [(a, Monomial (Variable x))]
toTerms = map swap . M.toList . unPoly

getCoefficients :: Poly x a -> [a]
getCoefficients = map fst . toTerms

coefficientOf :: (Ringoid a, Ord x) => Monomial (Variable x) -> Poly x a -> a
coefficientOf m p = fromMaybe rZero $ M.lookup m (unPoly p)



{-------------}
{- :Monadish -}
{-------------}

mapVar :: (Ord x) => (Monomial (Variable x) -> Monomial (Variable x)) -> Poly x a -> Poly x a
mapVar f = Poly . M.mapKeys f . unPoly

mapCoef :: (a -> b) -> Poly x a -> Poly x b
mapCoef f = Poly . M.map f . unPoly

polySeq :: (Ord x) => (Functor m, Monad m) => Poly x (m a) -> m (Poly x a)
polySeq = fmap (Poly . M.fromList) . sequence . map seqSnd . M.toList . unPoly



{-----------------}
{- :Constructors -}
{-----------------}

typeFixPoly :: x -> a -> Poly x a
typeFixPoly x a = undefined

zeroPoly :: (Ord x) => Poly x a
zeroPoly = Poly $ M.fromList []

fromTerm :: (Ord x) => (a, Monomial (Variable x)) -> Poly x a
fromTerm (a, x) = Poly $ M.fromList [(canon x, a)]

fromTerms :: (Ord x) => [(a, Monomial (Variable x))] -> Poly x a
fromTerms = Poly . M.fromList . map (\(a,x) -> (canon x, a))

reducedFromTerm :: (Ringoid a, Ord x) => (a, Monomial (Variable x)) -> Poly x a
reducedFromTerm (a, x) = if rIsZero a
  then zeroPoly
  else Poly $ M.fromList [(canon x, a)]

constPoly :: (Ringoid a, Ord x) => a -> Poly x a
constPoly c = fromTerm (c, identity)

varPoly :: (Ringoid a, URingoid a, Ord x) => Variable x -> Poly x a
varPoly x = fromTerm (rOne, variable x)

varsPoly :: (Ringoid a, URingoid a, Ord x) => [Variable x] -> Poly x a
varsPoly xs = fromTerm (rOne, makeMonomial $ map (\x -> (x, Nat 1)) xs)

onePoly :: (Ringoid a, URingoid a, Ord x) => Poly x a
onePoly = constPoly rOne

addPoly :: (Ringoid a, Ord x) => Poly x a -> Poly x a -> Either AlgErr (Poly x a)
addPoly p q = do
  let a = unPoly $ mapCoef return p
  let b = unPoly $ mapCoef return q
  polySeq $ Poly $ M.unionWith (opM2 rAdd) a b

sumPoly :: (Ringoid a, Ord x) => [Poly x a] -> Either AlgErr (Poly x a)
sumPoly = foldM addPoly zeroPoly

reducedFromTerms :: (Ringoid a, Ord x)
  => [(a, Monomial (Variable x))] -> Either AlgErr (Poly x a)
reducedFromTerms = sumPoly . map fromTerm . filter (\(c,_) -> not $ rIsZero c)

fromCoefficients :: (Ringoid a, Ord x)
  => Monomial (Variable x) -> [a] -> Either AlgErr (Poly x a)
fromCoefficients m cs = reducedFromTerms $ zip cs (powers m)



{-------------}
{- :Querying -}
{-------------}

coefficients :: (Ringoid a, Canon a, Ord x)
  => Poly x a -> Either AlgErr [a]
coefficients p = do
  q <- canonPoly p
  return $ map fst $ toTerms q

canonPoly :: (Ringoid a, Canon a, Ord x)
  => Poly x a -> Either AlgErr (Poly x a)
canonPoly = reducedFromTerms . map (\(a,x) -> (canon a, canon x)) . toTerms

equalPoly :: (Eq a, Ringoid a, Canon a, Ord x)
  => Poly x a -> Poly x a -> Bool
equalPoly p q = case check of
  Left _  -> False
  Right t -> t
  where
    check = do
      a <- canonPoly p
      b <- canonPoly q
      return $ (unPoly a) == (unPoly b)

isZeroPoly :: (Eq a, Ringoid a, Canon a, Ord x) => Poly x a -> Bool
isZeroPoly = equalPoly zeroPoly

monomials :: (Ringoid a, Canon a, Ord x)
  => Poly x a -> Either AlgErr [Monomial (Variable x)]
monomials p = do
  q <- canonPoly p
  return $ map snd $ toTerms q

variables :: (Ringoid a, Canon a, Ord x)
  => Poly x a -> Either AlgErr [Variable x]
variables p = do
  ms <- monomials p
  return $ nub $ concatMap monomialSupport ms

isUnivariate :: (Ringoid a, Canon a, Ord x)
  => Poly x a -> Either AlgErr Bool
isUnivariate p = do
  vars <- variables p
  case vars of
    []  -> return True
    [_] -> return True
    _   -> return False

isConstant :: (Ringoid a, Canon a, Ord x)
  => Poly x a -> Either AlgErr Bool
isConstant p = do
  vars <- variables p
  case vars of
    [] -> return True
    _  -> return False

contentPoly :: (Ringoid t, GCDoid t, Canon t, Ord x) => Poly x t -> Either AlgErr t
contentPoly p = canonPoly p >>= coefficients >>= rGCDs

-- List of terms using the supplied monomial order
termsBy :: (Monomial (Variable x) -> Monomial (Variable x) -> Ordering) -> Poly x a
  -> Either AlgErr [(a, Monomial (Variable x))]
termsBy ord p = do
  let foo (_,m1) (_,m2) = ord m1 m2
  return $ sortBy foo $ toTerms p

leadingTermBy :: (Ringoid a, Canon a, Eq a, Ord x)
  => (Monomial (Variable x) -> Monomial (Variable x) -> Ordering)
    -> Poly x a -> Either AlgErr (a, Monomial (Variable x))
leadingTermBy ord p = do
  q <- canonPoly p
  let foo (_,m1) (_,m2) = ord m1 m2
  if isZeroPoly q
    then Left ZeroPolynomial
    else return $ maximumBy foo $ toTerms p

leadingCoefBy :: (Ringoid a, Canon a, Eq a, Ord x)
  => (Monomial (Variable x) -> Monomial (Variable x) -> Ordering) -> Poly x a -> Either AlgErr a
leadingCoefBy ord p = do
  (a,_) <- leadingTermBy ord p
  return a

leadingDegreeBy :: (Ringoid a, Canon a, Eq a, Ord x)
  => (Monomial (Variable x) -> Monomial (Variable x) -> Ordering) -> Poly x a -> Either AlgErr Natural
leadingDegreeBy ord p = do
  (_,m) <- leadingTermBy ord p
  return $ degree m



{---------------}
{- :Arithmetic -}
{---------------}

mulMonomial :: (Ord x) => Monomial (Variable x) -> Poly x a -> Poly x a
mulMonomial x = mapVar (multiply x)

mulCoef :: (Ringoid a, CRingoid a, Ord x)
  => a -> Poly x a -> Either AlgErr (Poly x a)
mulCoef a = polySeq . mapCoef (rMul a)

mulTerm :: (Ringoid a, CRingoid a, Ord x)
  => (a, Monomial (Variable x)) -> Poly x a -> Either AlgErr (Poly x a)
mulTerm (a,x) p = mulCoef a $ mulMonomial x p

negPoly :: (Ringoid a) => Poly x a -> Poly x a
negPoly p = mapCoef rNeg p

multiplyPoly :: (Ringoid a, CRingoid a, Ord x)
  => Poly x a -> Poly x a -> Either AlgErr (Poly x a)
multiplyPoly p q = (sequence $ map (`mulTerm` p) $ toTerms q) >>= sumPoly

productPoly :: (Ringoid a, CRingoid a, URingoid a, Ord x)
  => [Poly x a] -> Either AlgErr (Poly x a)
productPoly = foldM multiplyPoly onePoly

powerPoly :: (Ringoid a, CRingoid a, URingoid a, Ord x)
  => Poly x a -> Integer -> Either AlgErr (Poly x a)
powerPoly p n = productPoly [p | _ <- [1..n]]

fromRoots :: (Ringoid a, CRingoid a, URingoid a, Ord x)
  => Variable x -> [a] -> Either AlgErr (Poly x a)
fromRoots x cs = do
  let foo c = addPoly (varPoly x) (negPoly $ constPoly c)
  ps <- sequence $ map foo cs
  productPoly ps

univariateLongDiv :: (Ringoid a, CRingoid a, URingoid a, Canon a, Eq a, Ord x)
  => Poly x a -> Poly x a -> Either AlgErr (Poly x a, Poly x a)
univariateLongDiv a' b' = do
  a <- canonPoly a'
  b <- canonPoly b'
  if isZeroPoly a
    then return (zeroPoly, zeroPoly)
    else do
      if isZeroPoly b
        then Left $ PolyDivByZero "univariateLongDiv 1"
        else do
          ta <- isUnivariate a
          tb <- isUnivariate b
          if not (ta && tb)
            then Left $ PolyDivErr "univariateLongDiv 2"
            else do
              u <- isConstant b
              if u
                then do
                  b0 <- toScalar b >>= rInv
                  q  <- rMul a (constPoly b0)
                  return (q, rZero)
                else do
                  v <- isConstant a
                  if v
                    then return (zeroPoly, a)
                    else do
                      [va] <- variables a
                      [vb] <- variables b
                      if va /= vb
                        then Left $ PolyDivErr "univariateLongDiv 3"
                        else do
                          n <- leadingDegreeBy mGLex a
                          m <- leadingDegreeBy mGLex b
                          if n < m
                            then return (zeroPoly, a)
                            else do
                              an <- leadingCoefBy mGLex a
                              bminv <- leadingCoefBy mGLex b >>= rInv
                              c <- rMul bminv an
                              t <- natSub n m
                              let h = reducedFromTerm (c, makeMonomial [(va, t)])
                              s <- rMul h b
                              abar <- rSub a s >>= canonPoly
                              (qbar, r) <- univariateLongDiv abar b
                              q <- rAdd qbar h >>= canonPoly
                              Right (q,r)



{---------------}
{- :Evaluation -}
{---------------}

toScalar :: (Ringoid a, Canon a, Eq a, Ord x)
  => Poly x a -> Either AlgErr a
toScalar q = do
  if isZeroPoly q
    then Right rZero
    else do
      t <- isConstant q
      if t
        then Right $ identity `coefficientOf` q
        else Left (PolyNotConstant "")


-- At a scalar

evalTermAtScalar :: (Ringoid a, CRingoid a, URingoid a, Ord x)
  => a -> Variable x -> (a, Monomial (Variable x)) -> Either AlgErr (a, Monomial (Variable x))
evalTermAtScalar c x (a,m) = do
  let Nat k = x `degreeOf` m
  b <- rPow c k
  d <- rMul a b
  return (d, removeVar x m)

evalPolyAtScalar :: (Ringoid a, CRingoid a, URingoid a, Canon a, Ord x)
  => a -> Variable x -> Poly x a -> Either AlgErr (Poly x a)
evalPolyAtScalar c x p = do
  us <- sequence $ map (evalTermAtScalar c x) (toTerms p)
  reducedFromTerms us

isRootOf :: (Ringoid a, CRingoid a, URingoid a, Canon a, Eq a, Ord x)
  => a -> Variable x -> Poly x a -> Bool
isRootOf a x p = case evalPolyAtScalar a x p of
  Left _  -> False
  Right q -> rIsZero q


-- At a polynomial

evalTermAtPoly :: (Ringoid a, CRingoid a, URingoid a, Ord x)
  => Poly x a -> Variable x -> (a, Monomial (Variable x)) -> Either AlgErr (Poly x a)
evalTermAtPoly p x (a,m) = do
  let Nat k = x `degreeOf` m
  q <- powerPoly p k
  let n = (a, removeVar x m)
  mulTerm n q

evalPolyAtPoly :: (Ringoid a, CRingoid a, URingoid a, Ord x)
  => Poly x a -> Variable x -> Poly x a -> Either AlgErr (Poly x a)
evalPolyAtPoly p x = join . fmap sumPoly . sequence . map (evalTermAtPoly p x) . toTerms

evalPolyAtPolys :: (Ringoid a, CRingoid a, URingoid a, Ord x)
  => [(Variable x, Poly x a)] -> Poly x a -> Either AlgErr (Poly x a)
evalPolyAtPolys [] p = Right p
evalPolyAtPolys ((x,q):xs) p = do
  s <- evalPolyAtPoly q x p
  evalPolyAtPolys xs s



{--------------}
{- :Instances -}
{--------------}

-- :Ringoid
instance (Ringoid a, CRingoid a, Canon a, Eq a, Ord x) => Ringoid (Poly x a) where
  rAdd p q = case addPoly p q of
    Left err -> Left (RingoidAddErr $ show err)
    Right x  -> return x

  rMul p q = case multiplyPoly p q of
    Left err -> Left (RingoidMulErr $ show err)
    Right x  -> return x

  rNeg = negPoly

  rIsZero = isZeroPoly

  rZero = zeroPoly
  rNeutOf _ = return zeroPoly
  rLAnnOf _ = return zeroPoly
  rRAnnOf _ = return zeroPoly


-- :CRingoid
instance (Ringoid a, CRingoid a, Ord x) => CRingoid (Poly x a)


-- :URingoid
instance (Ringoid a, CRingoid a, URingoid a, Canon a, Eq a, Ord x) => URingoid (Poly x a) where
  rOne = onePoly
  rIsOne = equalPoly onePoly

  rIsUnit = error "rIsUnit: Polynomial"

  rInv = error "rInv: Polynomial"

  rLOneOf _ = return onePoly
  rROneOf _ = return onePoly

  rInjInt n = do
    k <- rInjInt n
    return $ constPoly k


-- :EDoid
instance (Ringoid a, CRingoid a, URingoid a, Canon a, Eq a, Ord x) => EDoid (Poly x a) where
  rDivAlg = univariateLongDiv

  rNorm p = do
    t <- isUnivariate p
    if t
      then fmap unNat $ leadingDegreeBy mGLex p
      else Left PolyNotUnivariate


-- :GCDoid
instance (Ringoid a, CRingoid a, URingoid a, Canon a, Eq a, Ord x) => GCDoid (Poly x a) where
  rGCD = rEuclidGCD


-- :BDoid
instance (Ringoid a, CRingoid a, URingoid a, Canon a, Eq a, Ord x) => BDoid (Poly x a) where
  rBezout = rEuclidBezout



{-------------}
{- :Printing -}
{-------------}

-- Pretty-print a polynomial over any set
showByP :: (Show x, Ord x) => (Monomial (Variable x) -> Monomial (Variable x) -> Ordering)
  -> String -> (a -> String) -> Poly x a -> Either AlgErr String
showByP ord sep f p = do
  ts <- termsBy ord p
  return $ mung $ concat $ intersperse ";" $ map foo ts
  where
    foo (a,x) = f a ++ showSepBy sep show x

    mung str = if str == "" then "Null" else "Poly(" ++ str ++ ")"

showP :: (Show a, Show x, Ord x) => Poly x a -> Either AlgErr String
showP = showByP mRevLex "." show

showStrP :: (Ord x, Show x) => Poly x String -> Either AlgErr String
showStrP = showByP mRevLex "." id


-- Pretty-print a polynomial over a unital ring (e.g. ZZ/(n))
showByUP :: (Ringoid a, URingoid a, Canon a, Show x, Ord x)
  => String -> (Monomial (Variable x) -> Monomial (Variable x) -> Ordering) -> (a -> String)
    -> Poly x a -> Either AlgErr String
showByUP sep ord f p = do
  ts <- termsBy ord p
  return $ bar $ concat $ foo ts
    where
      bar [] = "0"
      bar s  = s

      foo [] = [""]
      foo (t:ts) = firstTerm t : map otherTerms ts

      firstTerm (a,m)
       | rIsZero a    = "0"
       | isIdentity m = f a
       | rIsOne a     = showSepBy sep show m
       | otherwise    = f a ++ showSepBy sep show m

      otherTerms (a,m)
       | rIsZero a                = ""
       | isIdentity m && rIsOne a = " + 1"
       | isIdentity m             = " + " ++ f a
       | rIsOne a                 = " + " ++ showSepBy sep show m
       | otherwise                = " + " ++ f a ++ showSepBy sep show m


showUP :: (Show a, Ringoid a, URingoid a, Canon a, Ord x, Show x)
  => Poly x a -> Either AlgErr String
showUP = showByUP "." mRevLex show


-- Pretty-print a polynomial over an ordered unital ring (e.g. ZZ)
showByOUP :: (Ringoid a, URingoid a, ORingoid a, Canon a, Show x, Ord x)
  => String -> (Monomial (Variable x) -> Monomial (Variable x) -> Ordering) -> (a -> String)
    -> Poly x a -> Either AlgErr String
showByOUP sep ord f p = do
  ts <- termsBy ord p
  return $ bar $ concat $ foo ts
    where
      bar [] = "0"
      bar s  = s

      foo [] = [""]
      foo (t:ts) = firstTerm t : map otherTerms ts

      firstTerm (a,m)
       | rIsZero a               = "0"

       | isIdentity m && rIsOne a    = "1"
       | isIdentity m && rIsPos a    = f (rAbs a)
       | isIdentity m && rIsNegOne a = "-1"
       | isIdentity m                = "-" ++ f (rAbs a)

       | rIsOne a    = showSepBy sep show m
       | rIsPos a    = f (rAbs a) ++ showSepBy sep show m
       | rIsNegOne a = "-" ++ showSepBy sep show m
       | otherwise   = "-" ++ f (rAbs a) ++ showSepBy sep show m

      otherTerms (a,m)
       | rIsZero a                = ""

       | isIdentity m && rIsOne a    = " + 1"
       | isIdentity m && rIsPos a    = " + " ++ f (rAbs a)
       | isIdentity m && rIsNegOne a = " - 1"
       | isIdentity m                = " - " ++ f (rAbs a)

       | rIsOne a    = " + " ++ showSepBy sep show m
       | rIsPos a    = " + " ++ f (rAbs a) ++ showSepBy sep show m
       | rIsNegOne a = " - " ++ showSepBy sep show m
       | otherwise   = " - " ++ f (rAbs a) ++ showSepBy sep show m

showOUP :: (Show a, Ringoid a, ORingoid a, URingoid a, Canon a, Ord x, Show x)
  => Poly x a -> Either AlgErr String
showOUP = showByOUP "." mRevLex show


instance (LaTeX a, Ringoid a, ORingoid a, URingoid a, Canon a, Ord x, Show x) => LaTeX (Poly x a) where
  latex p = case showByOUP "" mRevLex latex p of
    Left err  -> show err
    Right str -> str


instance (Ringoid a, Canon a, Ord x) => Canon (Poly x a) where
  canon = fromTerms . filter (\(a,_) -> not $ rIsZero a) . map (\(a,m) -> (canon a, m)) . toTerms
