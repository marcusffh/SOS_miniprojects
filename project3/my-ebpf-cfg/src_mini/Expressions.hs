-- Part 3

module Expressions where

import Interval
import Environment
import qualified Data.Map as Map
import Ebpf.Asm (Reg)

addInterval :: Interval -> Interval -> Interval
addInterval Bottom _ = Bottom
addInterval _ Bottom = Bottom
addInterval (Interval l1 u1) (Interval l2 u2) =
    Interval (addBound l1 l2) (addBound u1 u2)

addBound :: Bound -> Bound -> Bound
addBound NegInf _ = NegInf
addBound _ NegInf = NegInf
addBound PosInf _ = PosInf
addBound _ PosInf = PosInf
addBound (Finite x) (Finite y) = Finite (x + y)

subInterval :: Interval -> Interval -> Interval
subInterval Bottom _ = Bottom
subInterval _ Bottom = Bottom
subInterval (Interval l1 u1) (Interval l2 u2) =
    Interval (subBound l1 u2) (subBound u1 l2)

subBound :: Bound -> Bound -> Bound
subBound NegInf PosInf = NegInf
subBound NegInf _      = NegInf
subBound PosInf _      = PosInf
subBound _ PosInf      = NegInf
subBound (Finite _) NegInf = PosInf
subBound (Finite x) (Finite y) = Finite (x - y)

mulInterval :: Interval -> Interval -> Interval
mulInterval Bottom _ = Bottom
mulInterval _ Bottom = Bottom

mulInterval (Interval l1 u1) (Interval l2 u2) =
    case (l1, u1, l2, u2) of
        (Finite a, Finite b, Finite c, Finite d) ->
            let values = [a*c, a*d, b*c, b*d]
            in Interval (Finite (minimum values)) (Finite (maximum values))

        _ ->
            Interval NegInf PosInf


lookupMemory :: Memory -> Interval -> Interval
lookupMemory _ Bottom = Bottom
lookupMemory mem (Interval (Finite l) (Finite u)) =
    foldr joinInterval Bottom
        [ Map.findWithDefault Bottom (fromIntegral addr) mem
        | addr <- [l..u]
        ]
lookupMemory _ _ = Interval NegInf PosInf


data Truth
    = DefinitelyTrue
    | DefinitelyFalse
    | Unknown
    deriving (Show, Eq)


lessThan :: Interval -> Interval -> Truth
lessThan Bottom _ = Unknown
lessThan _ Bottom = Unknown
lessThan (Interval l1 u1) (Interval l2 u2)
    | u1 < l2  = DefinitelyTrue
    | l1 >= u2 = DefinitelyFalse
    | otherwise = Unknown

equal :: Interval -> Interval -> Truth
equal Bottom _ = Unknown
equal _ Bottom = Unknown
equal (Interval l1 u1) (Interval l2 u2)
    | u1 < l2 || u2 < l1 = DefinitelyFalse
    | l1 == u1 && l2 == u2 && l1 == l2 = DefinitelyTrue
    | otherwise = Unknown

greaterThan :: Interval -> Interval -> Truth
greaterThan x y = lessThan y x

lessEqual :: Interval -> Interval -> Truth
lessEqual Bottom _ = Unknown
lessEqual _ Bottom = Unknown
lessEqual (Interval l1 u1) (Interval l2 u2)
    | u1 <= l2 = DefinitelyTrue
    | l1 > u2 = DefinitelyFalse
    | otherwise = Unknown

greaterEqual :: Interval -> Interval -> Truth
greaterEqual x y = lessEqual y x

notEqual :: Interval -> Interval -> Truth
notEqual Bottom _ = Unknown
notEqual _ Bottom = Unknown
notEqual x y = case equal x y of DefinitelyTrue -> DefinitelyFalse; DefinitelyFalse -> DefinitelyTrue; Unknown -> Unknown

evalReg :: Environment -> Reg -> Interval
evalReg env r = Map.findWithDefault Bottom r env

evalConstant :: Integer -> Interval
evalConstant x = Interval (Finite x) (Finite x)

evalAddConstant :: Environment -> Reg -> Integer -> Interval
evalAddConstant env r n = addInterval (evalReg env r) (evalConstant n)

evalAdd :: Environment -> Reg -> Reg -> Interval
evalAdd env r1 r2 = addInterval (evalReg env r1) (evalReg env r2)

data Expr
    = RegExpr Reg -- an expression for a register
    | ConstExpr Integer -- just an integer
    | AddExpr Expr Expr
    | SubExpr Expr Expr
    | MulExpr Expr Expr
    | MemExpr Expr -- memory
    deriving (Show, Eq)


evalExpr :: Environment -> Memory -> Expr -> Interval
evalExpr env mem expr =
    case expr of
        RegExpr r ->
            evalReg env r

        ConstExpr n ->
            evalConstant n

        AddExpr e1 e2 ->
            addInterval (evalExpr env mem e1) (evalExpr env mem e2)

        SubExpr e1 e2 ->
            subInterval (evalExpr env mem e1) (evalExpr env mem e2)

        MulExpr e1 e2 ->
            mulInterval (evalExpr env mem e1) (evalExpr env mem e2)

        MemExpr address ->
            lookupMemory mem (evalExpr env mem address)
