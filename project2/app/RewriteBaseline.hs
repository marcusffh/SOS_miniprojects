-- Rewrite.hs is the main file we are supposed to implement for the assignment.
-- It currently contains an identity rewriter, meaning it basically returns the original program unchanged.
-- It already provides the structure for turning the program into labeled form, transforming it, and assembling it back into a normal program.
-- We need to replace the transform part with our actual SFI instrumentation: memory checks, control-flow protection, and an error stub.
-- This is where most of our actual implementation work will happen.


-- We transform a well-formed micro-Ebpf program into another well-formed program that satisfy:
--  1. Memory access must stay inside [DB, DL).
--  2. Control flow must stay inside the program

-- We tested on all example programs in examples/*.asm. All passed
module RewriteBaseline (rewrite) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Ebpf.Asm
import MicroEbpf.Layout
import MicroEbpf.Syntax
import MicroEbpf.WellFormed

-- Labels of the rewritten program: the address of an original
-- instruction, or a label of your own (extend as needed).
data L
  = Orig Int
  | ErrorStub
  deriving (Eq, Ord, Show)

rewrite :: Program -> Either String Program
rewrite p =
  assemble (prologue
         ++ concatMap transform (map relabel (labeled p))
         ++ [Label ErrorStub, Ins Error]) -- Put everything back together and calculate new jump offsets
  where
    prologue =
      [ Ins (Binary B32 Mov (Reg 11) (R (Reg 1)))
      , Ins (Binary B32 Mov (Reg 12) (R (Reg 10)))
      ]

    transform it = -- Take every original instruction and decide "do i need to add a safety check here?"
      case it of
        Ins (Load B32 d s moff) ->
          case moff of
            Nothing ->
              [ Ins (Binary B32 Mov (Reg 13) (R s))
              , JCondTo Jlt (Reg 13) (R (Reg 11)) ErrorStub
              , JCondTo Jge (Reg 13) (R (Reg 12)) ErrorStub
              , it --means "after checks pass, execute original instruction"
              ]

            Just off ->
              [ Ins (Binary B32 Mov (Reg 13) (R s))
              , Ins (Binary B32 Add (Reg 13) (Imm off))
              , JCondTo Jlt (Reg 13) (R (Reg 11)) ErrorStub
              , JCondTo Jge (Reg 13) (R (Reg 12)) ErrorStub
              , it
              ]

        Ins (Store B32 r moff (R s)) ->
          case moff of
            Nothing ->
              [ Ins (Binary B32 Mov (Reg 13) (R r))
              , JCondTo Jlt (Reg 13) (R (Reg 11)) ErrorStub
              , JCondTo Jge (Reg 13) (R (Reg 12)) ErrorStub
              , it
              ]

            Just off ->
              [ Ins (Binary B32 Mov (Reg 13) (R r))
              , Ins (Binary B32 Add (Reg 13) (Imm off))
              , JCondTo Jlt (Reg 13) (R (Reg 11)) ErrorStub
              , JCondTo Jge (Reg 13) (R (Reg 12)) ErrorStub
              , it
              ]
        
        -- CONDITIONAL JUMP
        Ins (JCond c r ri off) ->
          [JCondTo c r ri ErrorStub]
        
        -- UNCONDITIONAL JUMP
        Ins (Jmp off) ->
          [JmpTo ErrorStub]
        _ ->
          [it]


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
