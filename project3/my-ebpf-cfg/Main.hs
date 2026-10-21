{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified System.Environment as Sys
import qualified Data.Set as Set
import Text.Printf
import Data.Maybe (mapMaybe)

import Data.Text.Display

import Ebpf.Asm
import Ebpf.AsmParser
import Ebpf.Display ()
 
import CFG

import qualified Data.Map as Map
import Analysis
import Environment
import InformationFlow
import SecurityState
import Security

------------------- The following is just for visualisation ------------------------

cfgToDot :: CFG -> String
cfgToDot graph = Set.toList graph >>= showTrans
  where
    showTrans (x, NonCF i, y) = printf "  %d -> %d [label=\"%s\"];\n" x y (display i)
    showTrans (x, Unconditional, y) = printf "  %d -> %d [label=\"jmp\"];\n" x y
    showTrans (x, Assert c r ir, y) = printf "  %d -> %d [label=\"%s\"];\n" x y (showJump c r ir)
    showJump c r ir = display c <> " " <> display r <> ", " <> display ir

dotPrelude :: String
dotPrelude =
  "digraph cfg { \n"++
  "node [fontname=\"monospace\"];\n"++
  "node [shape=box];\n"++
  "edge [fontname=\"monospace\"];\n"

markNodes :: Program -> String
markNodes prog = concat $ mapMaybe mark $ label prog
  where
    mark (lab, Exit) = return $ printf "%d [style=\"rounded,filled\",fillcolor=grey];\n" lab
    mark (lab, JCond _ _ _ _) = return $ printf "%d [shape=diamond];\n" lab
    mark _ = Nothing

main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    [ebpfFile, dotFile] -> do
      res <- parseFromFile ebpfFile
      case res of
        Left err -> do
          putStrLn "Some sort of error occurred while parsing:"
          print err
        Right prog -> do
          print prog
          printf "The eBPF file %s has %d instructions\n" ebpfFile (length prog)

          let graph = cfg prog
              exists = exitNodes prog
              postDom = postDominators graph exists
              controlDeps = controlDependencies graph postDom
              initialValueState = State Map.empty Map.empty
              valueStates = analyze 0 graph (Map.singleton 0 initialValueState)
              initialSecurityState =
                setRegisterLevel (Reg 1) High emptySecurityState
              securityStates = analyzeSecurity graph valueStates
                (initialSecurityStates initialSecurityState)
              edges = cfgToDot graph
          
          print controlDeps
          print postDom
          print valueStates
          print securityStates
          
          writeFile dotFile (dotPrelude ++
                             edges ++
                             markNodes prog ++ "}")
          printf "Visualised the CFG in %s\n" dotFile
    _ ->
      putStrLn "Usage: ebpf-cfg <EBPF_FILE> <DOT_FILE>"
