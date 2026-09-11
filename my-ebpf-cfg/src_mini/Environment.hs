module Environment where

import Data.Map (Map)
import Interval
import Ebpf.Asm (Reg)

type Environment = Map Reg Interval
-- Take a register, such as r0, and map it to an interval

type Memory = Map Int Interval
-- Take a memory adress such as 0 or 25, and map it to an interval

data State = State Environment Memory
-- At each program point, our analysis state consist of both the register environment and the memory
