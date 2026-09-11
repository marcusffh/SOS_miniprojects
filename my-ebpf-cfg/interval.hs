data Bound
    = NegInf
    | Finite Integer
    | PosInf
    deriving (Show, Eq)

data Interval
    = Bottom
    | Interval Bound Bound
    deriving (Show, Eq)