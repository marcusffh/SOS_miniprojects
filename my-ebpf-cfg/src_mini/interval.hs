data Bound  -- define a type with three possible value
    = NegInf -- negative infinity
    | Finite Integer -- an actual number
    | PosInf -- positive infinity
    deriving (Show, Eq) --
    -- show lets you print things
    -- lets you check equality '=='

-- this block defines what each instance of comparring bounds equates
instance Ord Bound where -- ord is a type class for ordering and comparing things
    compare NegInf NegInf = EQ -- equal
    compare NegInf _      = LT -- negative infnitive is less than _
    compare PosInf PosInf = EQ -- equal
    compare PosInf _      = GT -- positive is greater than anything else
    compare (Finite x) (Finite y) = compare x y -- compare 
    compare (Finite _) PosInf = LT -- finite < positive infinity
    compare (Finite _) NegInf = GT -- finite > negative infinity

data Interval
    = Bottom -- represents the empty interval
    | Interval Bound Bound -- any other interval
    deriving (Show, Eq)


-- sqsubseteq notation
instance Ord Interval where
    Bottom <= _ = True
    _ <= Bottom = False
    Interval l1 u1 <= Interval l2 u2 =
        l2 <= l1 && u1 <= u2

-- A function that computes the smallest interval to contain both intervals
joinInterval :: Interval -> Interval -> Interval
joinInterval Bottom x = x
joinInterval x Bottom = x
joinInterval (Interval l1 u1) (Interval l2 u2) =
    Interval (min l1 l2) (max u1 u2)
