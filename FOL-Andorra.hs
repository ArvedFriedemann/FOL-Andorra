{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FOL_Andorra where

import Control.Monad.Var
data Decision = TOP | BOT
data Const = DEC Decision | UNKNOWN
data Term v a = CONST Const | ATOM a {-| VAR (PTerm v a) |-} APPL (PTerm v a) (PTerm v a)
deriving instance (Eq a, Eq (v a), Eq (v (PTerm v a))) => Eq (PTerm v a)

type PTerm v a = v (PTerm v a)

--processes a deconstructed equality. Returns conjunction of new equality constraints with the information whether they hold or not, possibly containin the inequality. Returns [] when constraint holds.
equality :: (Eq (PTerm v a)) =>
             PTerm v a ->
            (PTerm v a, Term v a) -> (PTerm v a, Term v a) ->
            m [(Decision, PTerm v a, PTerm v a)]
equality (pa, UNKNOWN) (pb, UNKNOWN) = return [(TOP, pa, pb)]
equality (pa, UNKNOWN) (pb, b) = put pa b >> return []
equality (pa, a) (pb, UNKNOWN) = put pb a >> return []
equality (pa, CONST x) (pb, CONST x')
  | x == x' = return []
  | otherwise = return [(BOT, pa, pb)]
equality (pa, ATOM x) (pb, ATOM x') =
  | x == x' = return []
  | otherwise = return [(BOT, pa, pb)]
{-
equality (pa, VAR x) (pb, VAR x') =
  | x == x' = return []
  | otherwise = return [(TOP, pa, pb)]

equality (pa, VAR x) (pb, _) = return [(TOP, x, pb)]
equality (pa, _) (pb, VAR x) = return [(TOP, pa, x)]
-}
equality (pa, APPL px py) (pb, APPL px' py') = return [(TOP, px, px'), (TOP, py, py')] --not sure if needed. If no constraint is being copied, are the tuples canonical?
equality (pa, _) (pb, _) = return [(BOT, pa, pb)]
