module Normalize where

import Lang 
import Pprinter
import Typing
import Parser

--checking if a term is closed
closed_aux :: Int -> Term -> Bool
closed_aux n (Ind m) = 0 < m && m <= n 
closed_aux n (Lam t) = closed_aux (n+1) t
closed_aux n (App t1 t2) = (closed_aux n t1) && (closed_aux n t2)
closed_aux n _ = True

closed :: Term -> Bool
closed = closed_aux 0

--for shifting indices
shift_aux :: Int -> Int -> Term -> Term
shift_aux ctxt n (Ind m) = if (m > ctxt) then (Ind (n+m)) else (Ind m)
shift_aux ctxt n (Lam t) = Lam (shift_aux (1+ctxt) n t)
shift_aux ctxt n (App t1 t2) = App (shift_aux ctxt n t1) (shift_aux ctxt n t2)
shift_aux ctxt n t = t

shift :: Int -> Term -> Term
shift = shift_aux 0

--substitution function, with first arg keeping track of # of lambda abstractions passed under
sub_aux :: Int -> Term -> Term -> Term
sub_aux n t (Ind m) 
    | m < n    = Ind m --index m is bound by a deeper lambda abstraction so leave it be
    | m == n    = (shift (n-1) t) --return t with fv's of t suitably incremented
    | otherwise = Ind (m-1) --decrement index to account for removal of outermost lambda abstraction
sub_aux n t (App t1 t2) = App (sub_aux n t t1) (sub_aux n t t2)
sub_aux n t (Lam t1) = Lam (sub_aux (n+1) t t1) --increment n to account for passing under new lambda abstraction
sub_aux _ _ t1 = t1 --constant terms

--the beta reduction (\x.M)N is performed as sub N M.  Since M is under one lambda abstraction, we begin with 1
sub :: Term -> Term -> Term
sub = sub_aux 1

beta :: Term -> Maybe Term
beta (App (Lam t1) t2) = Just (sub t2 t1)
beta _ = Nothing

normalize :: Term -> Term
normalize (App t1 t2) = case (normalize t1) of
                        (Lam t) -> let u = normalize t2 in normalize (sub u t)
                        Succ -> case normalize t2 of
                                 LitN n -> LitN (n+1)
                                 u -> App Succ u
                        App (App Iter (LitN n)) f -> if n == 0 then normalize t2 else normalize (App f (App (App (App Iter (LitN (n-1))) f) t2))
                        App (App If (LitB b)) x -> if b then x else normalize t2    
                        Pr1 -> case normalize t2 of
                                App (App Pair x) _ -> x
                                u -> App Pr1 u
                        Pr2 -> case normalize t2 of
                                App (App Pair _) y -> y
                                u -> App Pr2 u
                        App (App Case f) g -> case normalize t2 of
                                                        App I1 x -> normalize (App f x)
                                                        App I2 y -> normalize (App g y)
                                                        u -> App (App (App Case f) g) u
                        t -> App t (normalize t2)
normalize (Lam t) = Lam (normalize t)
normalize t = t