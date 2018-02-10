-- A Sum Monoid that also derivings Fractional and Floating

{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

module Sum (Sum(..)) where

newtype Sum a = Sum { getSum :: a }
    deriving (Show, Eq, Functor, Num, Fractional, Floating)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum a `mappend` Sum b = Sum $ a + b
