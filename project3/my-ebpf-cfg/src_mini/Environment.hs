module Environment where

import Data.Map (Map)
import qualified Data.Map as Map
import Interval
import Ebpf.Asm (Reg)

type Environment = Map Reg Interval
-- Take a register, such as r0, and map it to an interval

type Memory = Map Int Interval
-- Take a memory adress such as 0 or 25, and map it to an interval

data State = State Environment Memory
    deriving (Show, Eq)
-- At each program point, our analysis state consist of both the register environment and the memory

joinEnvironment :: Environment -> Environment -> Environment
joinEnvironment =
    Map.unionWith joinInterval

joinMemory :: Memory -> Memory -> Memory
joinMemory =
    Map.unionWith joinInterval

joinState :: State -> State -> State
joinState (State env1 mem1) (State env2 mem2) =
    State (joinEnvironment env1 env2)
          (joinMemory mem1 mem2)
