module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import MicroEbpf.Syntax
import MicroEbpf.WellFormed

import qualified RewriteBaseline as Baseline
import qualified RewriteMask as Mask

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["baseline", fin, fout] ->
      runRewriter Baseline.rewrite fin fout

    ["mask", fin, fout] ->
      runRewriter Mask.rewrite fin fout

    _ ->
      failWith "usage: sfi-rewrite (baseline|mask) IN.asm OUT.asm"

runRewriter finRewrite fin fout = do
  parsed <- parseProgramFile fin
  p <- either failWith return parsed

  either
    (\e -> failWith ("input is not well-formed: " ++ e))
    return
    (wellFormed Input p)

  p' <- either failWith return (finRewrite p)

  either
    (\e -> failWith ("BUG: output is not well-formed: " ++ e))
    return
    (wellFormed Output p')

  writeFile fout (showProgram p')

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr msg
  exitFailure