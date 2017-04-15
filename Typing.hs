module Typing where

import Data.List
import Lang
import Pprinter

tymap :: (Int -> Int) -> Ty -> Ty
tymap f (Var n) = Var (f n)
tymap f (Arrow s1 s2) = Arrow (tymap f s1) (tymap f s2)
tymap f (Prod s1 s2) = Prod (tymap f s1) (tymap f s2)
tymap f (Sum s1 s2) = Sum (tymap f s1) (tymap f s2)
tymap f s = s

--list of variables in a type in order of appearance in the syntax tree from left to right
varsTy :: Ty -> [Int]
varsTy (Var n) = [n]
varsTy (Arrow s1 s2) = union (varsTy s1) (varsTy s2)
varsTy (Prod s1 s2) = union (varsTy s1) (varsTy s2)
varsTy (Sum s1 s2) = union (varsTy s1) (varsTy s2)
varsTy _ = []

--reduces the indices of variables in types.  e.g. a7*a8 -> a8*a7 simplifies to a0*a1 -> a1*a0
simpleTy :: Ty -> Ty
simpleTy t = tymap (\n -> case elemIndex n (varsTy t) of
                            Nothing -> (-1) --shouldn't ever happen
                            Just m  -> m) t

itemAt :: Int -> [a] -> Maybe a
itemAt _ [] = Nothing
itemAt 0 (x:_) = Just x
itemAt n (_:xs) = itemAt (n-1) xs

contains :: Int -> Ty -> Bool
contains n (Var m)       = n == m
contains n (Arrow s1 s2) = (contains n s1) || (contains n s2)
contains n (Prod s1 s2)  = (contains n s1) || (contains n s2)
contains n (Sum s1 s2)   = (contains n s1) || (contains n s2)
contains _ _             = False

fpair :: (a -> a -> a) -> (b -> b -> b) -> (a,b) -> (a,b) -> (a,b)
fpair f g (a1,b1) (a2,b2) = (f a1 a2,g b1 b2)

didsub :: (Int,Ty) -> Ty -> (Bool,Ty)
didsub (n,s) (Var m) = if m == n then (True,s) else (False,Var m)
didsub p (Arrow s1 s2) = fpair (||) Arrow (didsub p s1) (didsub p s2)
didsub p (Prod s1 s2) = fpair (||) Prod (didsub p s1) (didsub p s2)
didsub p (Sum s1 s2) = fpair (||) Sum (didsub p s1) (didsub p s2)
didsub p s = (False,s)

fstof3 :: (a,b,c) -> a
fstof3 (a,_,_) = a

lst2 :: (a,b,c) -> (b,c)
lst2 (_,b,c) = (b,c)

tupsub :: (Int,Ty) -> (Bool,Ty,Ty) -> (Bool,Ty,Ty)
tupsub p (b,s1,s2) =    let (mod1,r1) = didsub p s1
                            (mod2,r2) = didsub p s2
                        in  (b || mod1 || mod2,r1,r2)

unify_aux :: [(Bool,Ty,Ty)] -> Maybe [(Ty,Ty)]
unify_aux [] = Just []
unify_aux ((False,s1,s2):rest) = case partition fstof3 rest of
                                    ([],feqs) -> Just ((s1,s2):(map lst2 feqs)) --nothing left to simplify
                                    (teqs,feqs) -> unify_aux (teqs ++ feqs)
unify_aux ((_,Var n,s):rest)
    | Var n == s    = unify_aux rest --delete
    | contains n s  = Nothing --check
    | otherwise     = unify_aux ((map (tupsub (n,s)) rest) ++ [(False,Var n,s)])
unify_aux ((_,s,Var n):rest) = unify_aux ((True,Var n,s):rest) --swap
unify_aux ((_,Arrow s1 s2,Arrow r1 r2):rest) = unify_aux ((True,s1,r1):(True,s2,r2):rest) --decompose
unify_aux ((_,Prod s1 s2,Prod r1 r2):rest) = unify_aux ((True,s1,r1):(True,s2,r2):rest)   --decompose
unify_aux ((_,Sum s1 s2,Sum r1 r2):rest) = unify_aux ((True,s1,r1):(True,s2,r2):rest)     --decompose
unify_aux ((_,N,N):rest) = unify_aux rest --delete
unify_aux ((_,B,B):rest) = unify_aux rest --delete
unify_aux ((_,U,U):rest) = unify_aux rest --delete
unify_aux _ = Nothing --conflict

unify :: [(Ty,Ty)] -> Maybe [(Ty,Ty)]
unify = unify_aux . (map (\(s1,s2) -> (True,s1,s2)))

eqns_aux :: [Ty] -> Int -> Ty -> Term -> Maybe (Int , [(Ty,Ty)])
eqns_aux ctxt n s (Ind m) = do
                                s1 <- itemAt (m-1) ctxt
                                return (n,[(s,s1)])
eqns_aux ctxt n s (App t1 t2) = do
                                    let sf = Var (n+1)
                                    (m,eqs1) <- eqns_aux ctxt (n+1) (Arrow sf s) t1
                                    (p,eqs2) <- eqns_aux ctxt m sf t2
                                    return (p,union eqs1 eqs2)
eqns_aux ctxt n s (Lam t) = do
                                let sf1 = Var (n+1)
                                let sf2 = Var (n+2)
                                (m,eqs) <- eqns_aux (sf1:ctxt) (n+2) sf2 t 
                                return (m,(s,Arrow sf1 sf2):eqs)
eqns_aux _ n s (LitN _) = Just (n,[(s,N)])
eqns_aux _ n s Succ = Just (n,[(s,Arrow N N)])
eqns_aux _ n s Iter = do
                        let sf = Var (n+1)
                        return ((n+1), [(s,Arrow N (Arrow (Arrow sf sf) (Arrow sf sf)))])
eqns_aux _ n s (LitB _) = Just (n,[(s,B)])
eqns_aux _ n s If = do
                        let sf = Var (n+1)
                        return (n+1,[(s,Arrow B (Arrow sf (Arrow sf sf)))])
eqns_aux _ n s Ast = Just (n,[(s,U)])
eqns_aux _ n s Pair = do
                        let sf1 = Var (n+1)
                        let sf2 = Var (n+2)
                        return (n+2,[(s,(Arrow sf1 (Arrow sf2 (Prod sf1 sf2))))])
eqns_aux _ n s Pr1 = do
                        let sf1 = Var (n+1)
                        let sf2 = Var (n+2)
                        return (n+2,[(s,(Arrow (Prod sf1 sf2) sf1))])
eqns_aux _ n s Pr2 = do
                        let sf1 = Var (n+1)
                        let sf2 = Var (n+2)
                        return (n+2,[(s,(Arrow (Prod sf1 sf2) sf2))])
eqns_aux _ n s I1 = do
                        let sf1 = Var (n+1)
                        let sf2 = Var (n+2)
                        return (n+2,[(s,Arrow sf1 (Sum sf1 sf2))])
eqns_aux _ n s I2 = do
                        let sf1 = Var (n+1)
                        let sf2 = Var (n+2)
                        return (n+2,[(s,Arrow sf2 (Sum sf1 sf2))])
eqns_aux _ n s Case = do
                        let sf1 = Var (n+1)
                        let sf2 = Var (n+2)
                        let sf3 = Var (n+3)
                        return (n+3,[(s,Arrow (Arrow sf1 sf3) (Arrow (Arrow sf2 sf3) (Arrow (Sum sf1 sf2) sf3)))])

eqns :: Term -> Maybe [(Ty,Ty)]
eqns t = fmap snd (eqns_aux [] 0 (Var 0) t)

maybeTy :: Term -> Maybe Ty
maybeTy t = do
                eqs <- eqns t
                subs <- unify eqs
                s <- lookup (Var 0) subs
                return (simpleTy s)
