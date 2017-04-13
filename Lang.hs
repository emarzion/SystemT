module Lang where

data Ty = N | B | U | Arrow Ty Ty | Prod Ty Ty | Sum Ty Ty | Var Int deriving (Eq,Show)

data Term =

    --naturals
      LitN Int
    | Succ
    | Iter

    --booleans
    | LitB Bool
    | If

    --unit
    | Ast

    --products
    | Pair
    | Pr1
    | Pr2

    --sums
    | I1 
    | I2 
    | Case

    --lambdas w/ De Bruijn indices
    | Ind Int
    | App Term Term
    | Lam Term

    deriving (Eq,Show)