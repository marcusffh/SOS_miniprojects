-- Rewrite.hs is the main file we are supposed to implement for the assignment
-- It currently contains an identity rewriter it basically returns the original program unchanged
-- It already provides the structure for turning the program into labeled form, transforming it, and assembling it back into a normal program
-- We need to replace the transform part with our actual SFI instrumentaion: memory checks, control-flow protection, and an error stub
-- This is where most of our actual implementation work will happen
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Ebpf.Asm
import MicroEbpf.Layout
import MicroEbpf.Syntax
import MicroEbpf.WellFormed

-- | Labels of the rewritten program: the address of an original
-- instruction, or a label of your own (extend as needed).
data L
  = Orig Int
  | ErrorStub
  deriving (Eq, Ord, Show)

    -- TODO: a prologue; a check in front of every load and store that is
    -- not safe by construction; a stub that ends the run with error; a
    -- treatment of the jumps whose target lies outside the code region.

rewrite :: Program -> Either String Program
rewrite p = assemble (prologue ++ concatMap transform (map relabel (labeled p)))
  where
    -- Save the original data base (DB) from r1.
    -- Registers r11-r15 are reserved for the inserted SFI code.
    prologue =
      [Ins (Binary B32 Mov (Reg 11) (R (Reg 1)))]

    -- Original instructions are unchanged for now.
    transform it = [it]

{- Reg 1       = r1
R (Reg 1)   = brug r1 som source
Reg 11      = r11
Mov          = kopier
B32          = 32-bit
Binary ...   = instruktionen
Ins ...      = læg instruktionen ind i programmet -}


relabel :: Item Int -> Item L
relabel it =
  case it of
    Label s -> Label (Orig s)
    Ins i -> Ins i
    JmpTo s -> JmpTo (Orig s)
    JCondTo c r ri s -> JCondTo c r ri (Orig s)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fin, fout] -> do
      parsed <- parseProgramFile fin
      p <- either failWith return parsed
      either (\e -> failWith ("input is not well-formed: " ++ e)) return (wellFormed Input p)
      p' <- either failWith return (rewrite p)
      either (\e -> failWith ("BUG: output is not well-formed: " ++ e)) return (wellFormed Output p')
      writeFile fout (showProgram p')
    _ -> failWith "usage: sfi-rewrite IN.asm OUT.asm"

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr msg
  exitFailure
