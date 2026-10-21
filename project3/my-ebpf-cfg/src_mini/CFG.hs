module CFG where

import Data.Set (Set)
import qualified Data.Set as Set

import Ebpf.Asm


data Trans =
    NonCF Instruction
  | Unconditional
  | Assert Jcmp Reg RegImm
  deriving (Show, Eq, Ord)

type Label = Int
type LabeledProgram = [(Int, Instruction)]
type CFG = Set (Label, Trans, Label)

label :: Program -> LabeledProgram
label = zip [0..]

neg :: Jcmp -> Jcmp
neg cmp =
  case cmp of
    Jeq -> Jne
    Jne -> Jeq
    Jgt -> Jle
    Jge -> Jlt
    Jlt -> Jge
    Jle -> Jgt
    Jsgt -> Jsle
    Jsge -> Jslt
    Jslt -> Jsge
    Jsle -> Jsgt
    Jset -> error "Don't know how to negate JSET"

cfg :: Program -> CFG
cfg prog = Set.unions $ map transfer $ label prog
  where
    transfer (i, instr) =
      case instr
        of
          JCond cmp r ir off ->
            Set.singleton (i, Assert cmp r ir, i+1+fromIntegral off)
            `Set.union`
            Set.singleton (i, Assert (neg cmp) r ir, i+1)

          Jmp off ->
            Set.singleton (i, Unconditional, i+1+fromIntegral off)

          Exit ->
            Set.empty

          _ ->
            Set.singleton (i, NonCF instr, i+1)
