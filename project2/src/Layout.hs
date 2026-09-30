-- | Code addresses of micro-eBPF programs, and a small assembler.
--
-- Every micro-eBPF instruction occupies one 64-bit unit of code, so code
-- addresses are instruction numbers: the instructions of a program @P@ are
-- at the addresses 0 .. #P-1 (the code region of @P@).  A jump at address
-- @a@ with offset @o@ transfers control to address @a + 1 + o@, which may
-- lie outside the code region (the semantics then ends the run in a
-- violation).
--
-- The assembler turns a list of items with symbolic labels into a program
-- with numeric offsets.  A rewriter can put the input program into labeled
-- form, insert code, and let 'assemble' recompute every jump offset.
module MicroEbpf.Layout
  ( addressed
  , jumpTarget
  , Item (..)
  , assemble
  , labeled
  ) where

import qualified Data.Map.Strict as M
import Ebpf.Asm

-- | Pair every instruction with its address.
addressed :: Program -> [(Int, Instruction)]
addressed prog = zip [0 ..] prog

-- | Target address of a jump at address @a@ (Nothing for other
-- instructions).  The target may lie outside the code region.
jumpTarget :: Int -> Instruction -> Maybe Int
jumpTarget a i =
  case i of
    JCond _ _ _ off -> Just ((a + 1) + fromIntegral off)
    Jmp off -> Just ((a + 1) + fromIntegral off)
    _ -> Nothing

-- | Items of a program with symbolic labels.  Jumps in 'Ins' items keep
-- their numeric offsets verbatim; use 'JmpTo' and 'JCondTo' for jumps that
-- 'assemble' should resolve.
data Item l
  = Label l
  | Ins Instruction
  | JmpTo l
  | JCondTo Jcmp Reg RegImm l
  deriving (Eq, Show)

itemSize :: Item l -> Int
itemSize it =
  case it of
    Label _ -> 0
    _ -> 1

-- | Resolve labels to offsets.  Fails on undefined or duplicate labels and
-- on offsets that do not fit in the signed 16-bit offset field.
assemble :: (Ord l, Show l) => [Item l] -> Either String Program
assemble items = do
  env <- collect M.empty 0 items
  emit env 0 items
  where
    collect env _ [] = Right env
    collect env a (it : rest) =
      case it of
        Label l
          | M.member l env -> Left ("duplicate label " ++ show l)
          | otherwise -> collect (M.insert l a env) a rest
        _ -> collect env (a + itemSize it) rest

    emit _ _ [] = Right []
    emit env a (it : rest) = do
      here <- resolve env a it
      more <- emit env (a + itemSize it) rest
      Right (here ++ more)

    resolve env a it =
      case it of
        Label _ -> Right []
        Ins i -> Right [i]
        JmpTo l -> do
          off <- offsetTo env a l
          Right [Jmp off]
        JCondTo c r ri l -> do
          off <- offsetTo env a l
          Right [JCond c r ri off]

    offsetTo env a l =
      case M.lookup l env of
        Nothing -> Left ("undefined label " ++ show l)
        Just t ->
          let off = t - (a + 1)
          in if (off < negate 32768) || (off > 32767)
               then Left ("jump offset out of range: " ++ show off)
               else Right (fromIntegral off)

-- | Labeled form of a program: a label (its address) before every
-- instruction, and every jump whose target lies in the code region
-- expressed with a symbolic target.  A jump whose target lies outside the
-- code region is kept as an 'Ins' item with its numeric offset: a rewriter
-- has to decide what to do with it.
labeled :: Program -> [Item Int]
labeled prog = concatMap item (addressed prog)
  where
    n = length prog
    inside t = (0 <= t) && (t < n)
    item (a, i) =
      Label a :
        case i of
          Jmp off | inside (tgt a off) -> [JmpTo (tgt a off)]
          JCond c r ri off | inside (tgt a off) -> [JCondTo c r ri (tgt a off)]
          _ -> [Ins i]
    tgt a off = (a + 1) + fromIntegral off
