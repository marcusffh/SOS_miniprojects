-- Securiy levels, ordering and joins.


module Security where
-- Defines our two security levels
-- Ord is so we can use the ordering Low < High
data SecurityLevel
  = Low
  | High
  deriving (Eq, Ord, Show)

--canFlowTo a b means information at level a is permitted to flow to level b
-- Public can flow into secret but secret cannot flow into public context
canFlowTo :: SecurityLevel -> SecurityLevel -> Bool
canFlowTo Low  Low  = True
canFlowTo Low  High = True
canFlowTo High High = True
canFlowTo High Low  = False

-- Join gives us the least upper bound of two security levels.
-- If input is High, the result is High
joinLevel :: SecurityLevel -> SecurityLevel -> SecurityLevel
joinLevel Low  Low  = Low
joinLevel Low  High = High
joinLevel High Low  = High
joinLevel High High = High

-- A helper for combining several levels
-- An empty list returns low, which is bottom element of our lattice
joinLevels :: [SecurityLevel] -> SecurityLevel
joinLevels = foldr joinLevel Low
