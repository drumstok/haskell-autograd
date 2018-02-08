-- Wrapping Sum to add some instances

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sum (Sum(..)) where

import qualified Data.Monoid as M (Sum(..))

newtype Sum a = Sum { getSum :: M.Sum a }
    deriving (Eq, Num, Functor, Monoid)

instance Show a => Show (Sum a) where
    show = show . M.getSum . getSum

instance Fractional a => Fractional (Sum a) where
    fromRational = Sum . M.Sum . fromRational
    recip = fmap recip

instance Floating a => Floating (Sum a) where
    pi = Sum $ M.Sum pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh
