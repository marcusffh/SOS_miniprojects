-- Rewrite.hs contains our SFI rewriter.
--
-- The baseline implementation protects memory accesses using explicit
-- bounds checks and also instruments control flow to keep execution
-- inside the allowed code region.
--
-- For our chosen extension, we replace the baseline memory bounds checks
-- with address masking. Control-flow protection is otherwise handled
-- separately from the address-masking extension.
--
-- For every memory access we compute:
--
--   safeAddr = DB + (EA & (size - 1))
--
-- where EA is the complete effective address and size = DL - DB.
--
-- This requires size to be a power of two and DB to be aligned to size.
-- Under these assumptions, the resulting address stays inside [DB, DL).
--
-- r11 stores DB, r12 stores the mask (size - 1), and r13 is used as a
-- scratch register while constructing the masked address.

module RewriteMask (rewrite) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Ebpf.Asm
import MicroEbpf.Layout
import MicroEbpf.Syntax
import MicroEbpf.WellFormed

-- Labels used while rewriting the program
-- Orig identifies an instruction from the original program, while
-- ErrorStub is the common error target used by the control-flow protection
data L
  = Orig Int
  | ErrorStub
  deriving (Eq, Ord, Show)

rewrite :: Program -> Either String Program
rewrite p =
  assemble (prologue
         ++ concatMap transform (map relabel (labeled p))
         ++ [Label ErrorStub, Ins Error])
  where
    -- Save the values needed by the masking instrumentation
    --
    -- Initially:
    --   r1 = DB
    --   r2 = DL - DB = size
    --
    -- After the prologue:
    --   r11 = DB
    --   r12 = size - 1 = mask
    --
    -- r11 and r12 can then be reused for every memory access
    prologue =
      [ Ins (Binary B32 Mov (Reg 11) (R (Reg 1)))
      , Ins (Binary B32 Mov (Reg 12) (R (Reg 2)))
      , Ins (Binary B32 Sub (Reg 12) (Imm 1))
      ]

    -- Rewrite instructions that require SFI instrumentation
    --
    -- Loads and stores are redirected through a masked address.
    -- r13 is used as a temporary register so that the address calculation
    -- does not modify the original address register
    transform it =
      case it of

        -- Load without an offset
        --
        -- Original:
        --   load d, [s]
        --
        -- Rewritten address:
        --   r13 = DB + (s & mask)
        Ins (Load B32 d s moff) ->
          case moff of
            Nothing ->
              [ Ins (Binary B32 Mov (Reg 13) (R s))
              , Ins (Binary B32 And (Reg 13) (R (Reg 12)))
              , Ins (Binary B32 Add (Reg 13) (R (Reg 11)))
              , Ins (Load B32 d (Reg 13) Nothing)
              ]

            -- Load with an offset
            --
            -- The complete effective address s + off must be calculated
            -- before masking:
            --
            --   EA   = s + off
            --   r13  = DB + (EA & mask)
            Just off ->
              [ Ins (Binary B32 Mov (Reg 13) (R s))
              , Ins (Binary B32 Add (Reg 13) (Imm off))
              , Ins (Binary B32 And (Reg 13) (R (Reg 12)))
              , Ins (Binary B32 Add (Reg 13) (R (Reg 11)))
              , Ins (Load B32 d (Reg 13) Nothing)
              ]

        -- Stores use the same masking scheme as loads
        -- r is the original address register and s contains the value
        -- that should be written
        Ins (Store B32 r moff (R s)) ->
          case moff of
            Nothing ->
              [ Ins (Binary B32 Mov (Reg 13) (R r))
              , Ins (Binary B32 And (Reg 13) (R (Reg 12)))
              , Ins (Binary B32 Add (Reg 13) (R (Reg 11)))
              , Ins (Store B32 (Reg 13) Nothing (R s))
              ]

            -- For a store with an offset, construct the complete effective
            -- address before applying the mask
            Just off ->
              [ Ins (Binary B32 Mov (Reg 13) (R r))
              , Ins (Binary B32 Add (Reg 13) (Imm off))
              , Ins (Binary B32 And (Reg 13) (R (Reg 12)))
              , Ins (Binary B32 Add (Reg 13) (R (Reg 11)))
              , Ins (Store B32 (Reg 13) Nothing (R s))
              ]

        -- Jumps whose original target is inside the code region have already
        -- been converted to JmpTo/JCondTo by labeled. We keep these symbolic
        -- targets unchanged, so assemble can recompute the correct offsets
        -- after the inserted instrumentation.

        JCondTo c r ri target ->
          [JCondTo c r ri target]

        JmpTo target ->
          [JmpTo target]

        -- If labeled leaves a jump as a numeric instruction, its original
        -- target lies outside the code region. Redirect it to ErrorStub so
        -- the rewritten program cannot end in a control-flow violation.
        Ins (JCond c r ri _) ->
          [JCondTo c r ri ErrorStub]

        Ins (Jmp _) ->
          [JmpTo ErrorStub]

        -- Instructions that do not require instrumentation are kept unchanged
        _ ->
          [it]


-- Convert labels referring to original instruction addresses into our
-- extended label type. This allows original labels and ErrorStub to coexist
-- while assemble calculates the final jump offsets
relabel :: Item Int -> Item L
relabel it =
  case it of
    Label s -> Label (Orig s)
    Ins i -> Ins i
    JmpTo s -> JmpTo (Orig s)
    JCondTo c r ri s -> JCondTo c r ri (Orig s)


failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr msg
  exitFailure