data Bound
    = NegInf
    | Finite Integer
    | PosInf
    deriving (Show, Eq)

instance Ord Bound where
    compare NegInf NegInf = EQ
    compare NegInf _      = LT
    compare PosInf PosInf = EQ
    compare PosInf _      = GT
    compare (Finite x) (Finite y) = compare x y
    compare (Finite _) PosInf = LT
    compare (Finite _) NegInf = GT

data Interval
    = Bottom
    | Interval Bound Bound
    deriving (Show, Eq)

instance Ord Interval where
    Bottom <= _ = True
    _ <= Bottom = False
    Interval l1 u1 <= Interval l2 u2 =
        l2 <= l1 && u1 <= u2

joinInterval :: Interval -> Interval -> Interval
joinInterval Bottom x = x
joinInterval x Bottom = x
joinInterval (Interval l1 u1) (Interval l2 u2) =
    Interval (min l1 l2) (max u1 u2)
