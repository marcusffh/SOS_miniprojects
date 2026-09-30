{-# LANGUAGE PatternSynonyms #-}
-- Semantics.hs sescribes what actually happens when a micro-eBPF instruction executes
-- It defines how registers, memory, jumps, loads, stores, arithmetic, etc. change the program state.
-- It also defines the different ways a program can finish, such as exit, error, trap, or violation
-- In particular, it tells us what happens when a program accesses memory outside the data region or jumps outside its code
-- For our project: This is what we use to understand what behavior our rewriteen program must preserve
module MicroEbpf.Semantics
  ( Final (..)
  , Mem (..)
  , memRead
  , memChanges
  , Config (..)
  , initialConfig
  , Step (..)
  , step
  , Outcome (..)
  , Stats (..)
  , run
  , runTrace
  , TraceEntry (..)
  , alu
  , compareJ
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Word (Word32)
import Numeric (showHex)
import Ebpf.Asm
import MicroEbpf.Contract
import MicroEbpf.Syntax (pattern Error)

-- | The four kinds of final configuration.
data Final = FExit | FError | FTrap | FViolation
  deriving (Eq, Show)

-- | Memory: the initial fill, and the words that differ from it.
data Mem = Mem
  { memFill :: !Fill
  , memWords :: !(IM.IntMap Word32)
  }

memRead :: Mem -> Word32 -> Word32
memRead m x = IM.findWithDefault (fillWord (memFill m) x) (fromIntegral x) (memWords m)

memWrite :: Mem -> Word32 -> Word32 -> Mem
memWrite m x v = m { memWords = IM.insert (fromIntegral x) v (memWords m) }

-- | The words of a memory that differ from its fill, in address order.
-- Two memories with the same fill are equal if and only if their changes
-- are equal.
memChanges :: Mem -> [(Word32, Word32)]
memChanges m =
  [ (fromIntegral x, v) | (x, v) <- IM.toList (memWords m), v /= fillWord (memFill m) (fromIntegral x) ]

-- | A running configuration <a, R, H>.
data Config = Config
  { cPc :: !Int                    -- ^ program pointer a
  , cRegs :: !(IM.IntMap Word32)   -- ^ registers R (r0-r15)
  , cMem :: !Mem                   -- ^ memory H
  }

initialConfig :: InitState -> Config
initialConfig st =
  Config
    { cPc = 0
    , cRegs = IM.fromList (M.toList (initRegisters st))
    , cMem = Mem (iFill st) (IM.fromList [ (fromIntegral x, v) | (x, v) <- M.toList (iWords st) ])
    }

-- | One step: a new running configuration, or a final configuration
-- (whose registers and memory are those of the configuration before the
-- step) with an explanation; 'Stuck' for an instruction that is not part
-- of micro-eBPF.
data Step
  = Next Config
  | Stop Final String
  | Stuck String

-- | The transition relation of the semantics, for a program given as a
-- map from addresses to instructions, its length #P and the data region.
step :: IM.IntMap Instruction -> Int -> Region -> Config -> Step
step code n region c =
  case IM.lookup a code of
    Nothing -> Stuck ("the program pointer " ++ (show a ++ " lies outside the code region"))
    Just i ->
      case i of
        Binary _ op (Reg d) ri -> goto (a + 1) (c { cRegs = IM.insert d (alu op (reg d) (val ri)) regs })
        Load B32 (Reg d) (Reg s) moff ->
          access (reg s + offset moff) (\x -> goto (a + 1) (c { cRegs = IM.insert d (memRead (cMem c) x) regs }))
        Store B32 (Reg d) moff (R (Reg s)) ->
          access (reg d + offset moff) (\x -> goto (a + 1) (c { cMem = memWrite (cMem c) x (reg s) }))
        JCond cmp (Reg d) ri off ->
          if compareJ cmp (reg d) (val ri)
            then goto ((a + 1) + fromIntegral off) c
            else goto (a + 1) c
        Jmp off -> goto ((a + 1) + fromIntegral off) c
        Exit -> Stop FExit "exit"
        Error -> Stop FError "error instruction"
        _ -> Stuck "not an instruction of micro-eBPF"
  where
    a = cPc c
    regs = cRegs c
    reg r = IM.findWithDefault 0 r regs
    val ri =
      case ri of
        R (Reg r) -> reg r
        Imm k -> fromIntegral k
    offset moff = maybe 0 fromIntegral moff

    -- the condition on the new program pointer
    goto a' c'
      | (0 <= a') && (a' < n) = Next (c' { cPc = a' })
      | a' == n = Stop FViolation ("violation: control passes to address " ++ (show a' ++ (", just past the code region [0, " ++ (show n ++ ")"))))
      | otherwise = Stop FViolation ("violation: jump to address " ++ (show a' ++ (", outside the code region [0, " ++ (show n ++ ")"))))

    -- the condition on the address of a load or store, then alignment
    access x k
      | not (inRegion region x) =
          Stop FViolation ("violation: address 0x" ++ (showHex x (" lies outside the data region [0x" ++ (showHex (rBase region) (", 0x" ++ (showHex (rLimit region) ")"))))))
      | (x .&. 3) /= 0 =
          Stop FTrap ("trap: address 0x" ++ (showHex x " lies in the data region but is not a multiple of 4"))
      | otherwise = k x

-- | The arithmetic operations on 32-bit words.
alu :: BinAlu -> Word32 -> Word32 -> Word32
alu op x y =
  case op of
    Add -> x + y
    Sub -> x - y
    Mul -> x * y
    Div -> if y == 0 then 0 else x `quot` y
    Mod -> if y == 0 then x else x `rem` y
    Or -> x .|. y
    And -> x .&. y
    Xor -> x `xor` y
    Lsh -> x `shiftL` fromIntegral (y .&. 31)
    Rsh -> x `shiftR` fromIntegral (y .&. 31)
    Arsh -> fromIntegral ((fromIntegral x :: Int32) `shiftR` fromIntegral (y .&. 31))
    Mov -> y

-- | The comparisons of the conditional jumps: unsigned, or signed on the
-- two's complement values (jsgt, jsge, jslt, jsle).
compareJ :: Jcmp -> Word32 -> Word32 -> Bool
compareJ cmp x y =
  let sx = fromIntegral x :: Int32
      sy = fromIntegral y :: Int32
  in case cmp of
       Jeq -> x == y
       Jne -> x /= y
       Jgt -> x > y
       Jge -> x >= y
       Jlt -> x < y
       Jle -> x <= y
       Jset -> (x .&. y) /= 0
       Jsgt -> sx > sy
       Jsge -> sx >= sy
       Jslt -> sx < sy
       Jsle -> sx <= sy

------------------------------------------------------------------------
-- Runs

-- | How a run ends: in a final configuration <f, R, H>, by exhausting the
-- step budget, or at an instruction that is not part of micro-eBPF.
data Outcome
  = Terminated Final (IM.IntMap Word32) Mem
  | OutOfSteps
  | StuckAt Int String

data Stats = Stats
  { stSteps :: !Int     -- ^ instructions executed
  , stLoads :: !Int
  , stStores :: !Int
  }
  deriving (Eq, Show)

data TraceEntry = TraceEntry Int Instruction (IM.IntMap Word32)

-- | Run a program from an initial state with a step budget.
run :: Int -> Program -> InitState -> (Outcome, Stats)
run fuel prog st =
  let (o, s, _, _) = execute False fuel prog st in (o, s)

-- | Like 'run', but also return the last executed instructions (at most
-- the given number) with the registers before each, and an explanation of
-- how the run ended.
runTrace :: Int -> Int -> Program -> InitState -> (Outcome, Stats, [TraceEntry], String)
runTrace keep fuel prog st =
  let (o, s, tr, why) = execute True fuel prog st
  in (o, s, reverse (take keep tr), why)

execute :: Bool -> Int -> Program -> InitState -> (Outcome, Stats, [TraceEntry], String)
execute tracing fuel prog st = loop (initialConfig st) (Stats 0 0 0) []
  where
    code = IM.fromList (zip [0 ..] prog)
    n = length prog
    region = iRegion st

    loop c stats tr
      | stSteps stats >= fuel = (OutOfSteps, stats, tr, "the step budget is exhausted")
      | otherwise =
          let a = cPc c
              i = IM.findWithDefault Exit a code
              stats' = stats { stSteps = stSteps stats + 1 }
              tr' = if tracing then TraceEntry a i (cRegs c) : take 200 tr else tr
          in case step code n region c of
               -- a load or store counts as a memory access only if it is
               -- performed (not when it ends in trap or violation)
               Next c' -> loop c' (count i stats') tr'
               Stop f why -> (Terminated f (cRegs c) (cMem c), stats', tr', why ++ (" (pc " ++ (show a ++ ")")))
               Stuck why -> (StuckAt a why, stats, tr', why ++ (" (pc " ++ (show a ++ ")")))

    count i s =
      case i of
        Load {} -> s { stLoads = stLoads s + 1 }
        Store {} -> s { stStores = stStores s + 1 }
        _ -> s
