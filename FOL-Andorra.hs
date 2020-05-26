{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FOL_Andorra where

import Control.Monad.Var

data Const = TOP | BOT | UNKNOWN
data Term v a = CONST Const | ATOM a | VAR (PTerm v a) | APPL (PTerm v a) (PTerm v a)
deriving instance (Eq a, Eq (v a), Eq (v (PTerm v a))) => Eq (PTerm v a)

type PTerm v a = v (PTerm v a)

--processes a deconstructed equality. Returns conjunction of new constraints, possibly containin the inequality.
equality :: (Eq (PTerm v a)) =>
            (PTerm v a -> PTerm v a -> m (PTerm v a)) ->
            (PTerm v a -> PTerm v a -> m (PTerm v a)) ->
             PTerm v a ->
            (PTerm v a, Term v a) -> (PTerm v a, Term v a) ->
            m [PTerm v a]
equality eqconst ineqconst this (pa, UNKNOWN) (pb, UNKNOWN) = return $ Just [this]
equality eqconst ineqconst this (pa, UNKNOWN) (pb, b) = --assign pa to b
equality eqconst ineqconst this (pa, a) (pb, UNKNOWN) = --assign pb to a
equality eqconst ineqconst this (pa, CONST x) (pb, CONST x')
  | x == x' = return []
  | otherwise = return [ineqconst pa pb]
equality eqconst ineqconst this (pa, ATOM x) (pb, ATOM x') = --case split on x==x'
  | x == x' = return []
  | otherwise = return [ineqconst pa pb]
equality eqconst ineqconst this (pa, VAR x) (pb, VAR x') =
  | x == x' = return []
  | otherwise = return [eqconst x x']
equality eqconst ineqconst this (pa, VAR x) (pb, _) = return [eqconst x pb]
equality eqconst ineqconst this (pa, _) (pb, VAR x) = return [eqconst x pa]

equality eqconst ineqconst this (pa, APPL px py) (pb, APPL px' py') = return [eqconst px px', eqconst py py']
equality eqconst ineqconst this (pa, _) (pb, _) = return [ineqconst pa pb]
