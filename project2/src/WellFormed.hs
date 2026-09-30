{-# LANGUAGE PatternSynonyms #-}
-- | Well-formedness of micro-eBPF programs (conditions W1-W3 of the
-- reference definition).
--
-- A program is well-formed with k registers if
--
-- * W1: it has at least one instruction; every instruction is one of
--   micro-eBPF (arithmetic with a register or an immediate operand,
--   @ldxw@, @stxw@, @ja@, the conditional jumps, @exit@, @error@); it
--   names only the registers r0 .. r(k-1); immediates lie in
--   [-2^31, 2^32); memory and jump offsets lie in [-2^15, 2^15);
--
-- * W2: no instruction writes r10 (the frame pointer);
--
-- * W3: the last instruction is @ja@, @exit@ or @error@.
--
-- Input programs have k = 11 registers (r0-r10); the output programs of a
-- rewriter have k = 16 (r0-r15).  Jump targets are not restricted: a jump
-- to an address outside the code region is well-formed, and the semantics
-- ends a run that takes it in a violation.
module MicroEbpf.WellFormed
  ( Dialect (..)
  , registerCount
  , wellFormed
  , writtenRegister
  , registersOf
  , jumpsOutside
  ) where

import Control.Monad (unless, when)
import Ebpf.Asm
import MicroEbpf.Layout
import MicroEbpf.Syntax (pattern Error, isMicroEbpf, showInstruction)

-- | Input programs (11 registers) and output programs (16 registers).
data Dialect = Input | Output
  deriving (Eq, Show)

registerCount :: Dialect -> Int
registerCount d =
  case d of
    Input -> 11
    Output -> 16

-- | The register an instruction writes, if any.
writtenRegister :: Instruction -> Maybe Reg
writtenRegister i =
  case i of
    Binary _ _ r _ -> Just r
    Unary _ _ r -> Just r
    Load _ r _ _ -> Just r
    LoadImm r _ -> Just r
    LoadMapFd r _ -> Just r
    _ -> Nothing

-- | W1-W3.  The result names the first offending instruction.
wellFormed :: Dialect -> Program -> Either String ()
wellFormed dialect prog = do
  when (null prog) (Left "W1: empty program")
  mapM_ checkOne (addressed prog)
  case last prog of
    Exit -> Right ()
    Jmp _ -> Right ()
    Error -> Right ()
    i -> Left ("W3: the last instruction must be ja, exit or error, not " ++ showInstruction i)
  where
    k = registerCount dialect

    checkOne (a, i) =
      case check i of
        Right () -> Right ()
        Left msg -> Left (msg ++ "  (pc " ++ (show a ++ (": " ++ (showInstruction i ++ ")"))))

    check i = do
      checkInstruction i
      mapM_ checkReg (registersOf i)
      checkImmediates i
      case writtenRegister i of
        Just (Reg 10) -> Left "W2: r10 is read-only"
        _ -> Right ()

    checkReg (Reg n) =
      unless ((0 <= n) && (n < k))
        (Left ("W1: invalid register r" ++ (show n ++ (" (registers are r0-r" ++ (show (k - 1) ++ ")")))))

    imm k' = (negate (2 ^ (31 :: Int)) <= k') && (k' < (2 ^ (32 :: Int)))
    off16 o = (negate 32768 <= o) && (o <= 32767)

    checkImm k' = unless (imm k') (Left ("W1: immediate out of range [-2^31, 2^32): " ++ show k'))
    checkOff o = unless (off16 o) (Left ("W1: offset out of range [-2^15, 2^15): " ++ show o))

    checkRegImm ri =
      case ri of
        Imm k' -> checkImm k'
        R _ -> Right ()

    checkImmediates i =
      case i of
        Binary _ _ _ ri -> checkRegImm ri
        Store _ _ moff _ -> mapM_ checkOff moff
        Load _ _ _ moff -> mapM_ checkOff moff
        JCond _ _ ri off -> do
          checkRegImm ri
          checkOff off
        Jmp off -> checkOff off
        _ -> Right ()

    checkInstruction i =
      unless (isMicroEbpf i) (Left "W1: not an instruction of micro-eBPF")

-- | The registers an instruction names.
registersOf :: Instruction -> [Reg]
registersOf i =
  case i of
    Binary _ _ r ri -> r : regOf ri
    Unary _ _ r -> [r]
    Store _ r _ ri -> r : regOf ri
    Load _ d s _ -> [d, s]
    LoadImm r _ -> [r]
    LoadMapFd r _ -> [r]
    LoadAbs _ _ -> []
    LoadInd _ r _ -> [r]
    JCond _ r ri _ -> r : regOf ri
    Jmp _ -> []
    Call _ -> []
    Exit -> []
  where
    regOf ri =
      case ri of
        R r -> [r]
        Imm _ -> []

-- | The jumps whose target lies outside the code region: (address,
-- target).  Such jumps are well-formed; a run that takes one ends in a
-- violation.
jumpsOutside :: Program -> [(Int, Int)]
jumpsOutside prog =
  [ (a, t) | (a, i) <- addressed prog, Just t <- [jumpTarget a i], (t < 0) || (t >= length prog) ]
