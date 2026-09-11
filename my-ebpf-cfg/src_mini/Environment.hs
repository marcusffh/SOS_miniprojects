type Environment = Map Reg Interval
type Memory = Map Int Interval

data State = State Environment Memory
