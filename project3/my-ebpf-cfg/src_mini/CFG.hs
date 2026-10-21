module CFG where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
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

nodes :: CFG -> Set Label
nodes graph =
    Set.fromList
        [ node
        | (source, _, destination) <- Set.toList graph
        , node <- [source, destination]
        ]

exitNodes :: Program -> Set Label
exitNodes prog =
    Set.fromList
        [ i
        | (i, Exit) <- label prog
        ]

postDominators :: CFG -> Set Label -> Map.Map Label (Set Label)
postDominators graph exits =
    let allNodes = nodes graph
        initial =
            Map.fromSet
                (\node ->
                    if Set.member node exits
                    then Set.singleton node
                    else allNodes
                )
                allNodes
    in iteratePostDominators graph exits initial

iteratePostDominators
    :: CFG
    -> Set Label
    -> Map.Map Label (Set Label)
    -> Map.Map Label (Set Label)
iteratePostDominators graph exits current =
    let next =
            Map.mapWithKey (updateNode graph exits current) current
    in if next == current
       then current
       else iteratePostDominators graph exits next

updateNode
    :: CFG
    -> Set Label
    -> Map.Map Label (Set Label)
    -> Label
    -> Set Label
    -> Set Label
updateNode graph exits current node _oldSet =
    if Set.member node exits
    then Set.singleton node
    else
        let successors =
                [ destination
                | (source, _, destination) <- Set.toList graph
                , source == node
                ]
            successorSets =
                [ Map.findWithDefault Set.empty successor current
                | successor <- successors
                ]
        in case successorSets of
            [] -> Set.singleton node
            firstSet:rest ->
                Set.insert node
                    (foldl Set.intersection firstSet rest)

controlDependencies
    :: CFG
    -> Map.Map Label (Set Label)
    -> Map.Map Label (Set Label)
controlDependencies graph postDom =
    Map.fromListWith Set.union
        [ (branch, controlledNodes)
        | (branch, Assert _ _ _, successor) <- Set.toList graph
        , let successorPostDom =
                  Map.findWithDefault Set.empty successor postDom
        , let branchPostDom =
                  Map.findWithDefault Set.empty branch postDom
        , let controlledNodes =
                  Set.difference successorPostDom branchPostDom
        , not (Set.null controlledNodes)
        ]
