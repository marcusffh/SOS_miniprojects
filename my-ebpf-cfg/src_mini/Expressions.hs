module Expressions where

import Interval

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
