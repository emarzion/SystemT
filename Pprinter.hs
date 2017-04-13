module Pprinter where

import Lang

--Pretty Printing for Types

tvar :: Int -> String
tvar n = "a" ++ (show n)

data Constr = Ar | Sm | Pr | At deriving (Eq,Show,Ord)

constr :: Ty -> Constr
constr (Arrow _ _) = Ar
constr (Sum _ _) = Sm
constr (Prod _ _) = Pr
constr _ = At 

sym :: Constr -> String
sym Ar = " -> "
sym Sm = " + "
sym Pr = " * "
sym At = "" --shouldn't be used

toList :: Constr -> Ty -> ([Ty],Ty)
toList Ar (Arrow t1 t2)  = case (toList Ar t2) of
                            (ts,t) -> (t1:ts,t)
toList Ar t = ([],t)
toList Sm (Sum t1 t2) = case (toList Sm t2) of
                            (ts,t) -> (t1:ts,t)
toList Sm t = ([],t)
toList Pr (Prod t1 t2) = case (toList Pr t2) of
                            (ts,t) -> (t1:ts,t)
toList _ t = ([],t)

addParens :: Constr ->  (Ty -> String) -> Ty -> String
addParens c f t 
    | constr t <= c = "(" ++ (f t) ++ ")"
    | otherwise     = f t

printList :: Constr -> (Ty -> String) -> [Ty] -> String
printList _ _ [] = ""
printList c f (t:ts) = (addParens c f t) ++ (sym c) ++ (printList c f ts)

typprint :: Ty -> String
typprint N = "N"
typprint B = "B"
typprint U = "U"
typprint (Var v) = tvar v
typprint t = case (toList (constr t) t) of
                (ts,t1) -> (printList (constr t) typprint ts) ++ (addParens (constr t) typprint t1)

--Pretty Printing for Terms

needsParens :: Term -> Bool
needsParens (App (App Pair _) _) = False
needsParens (App _ _) = True
needsParens (Lam _) = True
needsParens _  = False

var :: Int -> String 
var n
    | n < 0     = "y" ++ (show (abs n))
    | otherwise = "x" ++ (show n)


getVars_aux :: Int -> Term -> ([String],Term)
getVars_aux n (Lam t1) = case getVars_aux (n+1) t1 of
                            (vs,t) -> ((var n):vs,t)
getVars_aux _ t = ([],t)

getVars :: Term -> ([String],Term)
getVars = getVars_aux 0

parensNonAtom :: Int -> (Int -> Term -> String) -> Term -> String
parensNonAtom n f t = if (needsParens t) then "(" ++ (f n t) ++ ")" else f n t

parensFun :: Int -> (Int -> Term -> String) -> Term -> String
parensFun n f (Lam t1) = "(" ++ (f n (Lam t1)) ++ ")"
parensFun n f t         = f n t

printVars :: [String] -> String
printVars [] = ""
printVars [v] = v
printVars (v:vs) = v ++ " " ++ (printVars vs)

pprint_aux :: Int -> Term -> String
pprint_aux n (Ind m) = var (n-m)
pprint_aux n (App (Ind m) t) = var (n-m) ++ " " ++ (parensNonAtom n pprint_aux t)
pprint_aux n (App (App Pair x) y) = "[" ++ (pprint_aux n x) ++ "," ++ (pprint_aux n y) ++ "]"
pprint_aux n (App (App t1 t2) t3) = (parensFun n pprint_aux t1) ++ " " ++ (parensNonAtom n pprint_aux t2) ++ " " ++ (parensNonAtom n pprint_aux t3)
pprint_aux n (App t1 t2) = (parensFun n pprint_aux t1) ++ " " ++ (parensNonAtom n pprint_aux t2)
pprint_aux n (Lam t) = case getVars_aux n (Lam t) of
                                (vs,t1) -> "fun " ++ (printVars vs) ++ " => " ++ (pprint_aux (n+length vs) t1)
pprint_aux _ (LitN n) = show n 
pprint_aux _ Succ = "S"
pprint_aux _ Iter = "iter"
pprint_aux _ If = "if"
pprint_aux _ Ast = "tt"
pprint_aux _ Pair = "pair"
pprint_aux _ (LitB b) = if b then "true" else "false"
pprint_aux _ Pr1 = "p1"
pprint_aux _ Pr2 = "p2"
pprint_aux _ I1 = "i1"
pprint_aux _ I2 = "i2"
pprint_aux _ Case = "case"

pprint :: Term -> String
pprint = pprint_aux 0

{-
mpprint :: Maybe Term -> String
mpprint Nothing = "Nothing"
mpprint (Just t) = pprint t
-}