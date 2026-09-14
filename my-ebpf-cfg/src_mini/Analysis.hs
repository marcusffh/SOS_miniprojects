module Analysis where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Interval
import Environment
import Expressions
import Ebpf.Asm
import CFG

initialStates :: Label -> State -> Map.Map Label State
initialStates entry initialState =
    Map.singleton entry initialState

incomingState :: CFG -> Map.Map Label State -> Label -> State
incomingState graph states target =
    joinStates
        [ newState
        | (source, trans) <- predecessors graph target
        , Just sourceState <- [Map.lookup source states]
        , Just newState <- [applyEdge sourceState (source, trans, target)]
        ]

applyEdge :: State -> (Label, Trans, Label) -> Maybe State
applyEdge state (_, trans, _) =
    transfer trans state

joinStates :: [State] -> State
joinStates [] = State Map.empty Map.empty
joinStates (s:ss) = foldl joinState s ss

predecessors :: CFG -> Label -> [(Label, Trans)]
predecessors graph target =
    [ (source, trans)
    | (source, trans, destination) <- Set.toList graph
    , destination == target
    ]

getEnv :: State -> Environment
getEnv (State env _) = env

getMemory :: State -> Memory
getMemory (State _ mem) = mem

updateReg :: State -> Reg -> Interval -> State
updateReg (State env mem) r value =
    State (Map.insert r value env) mem

updateMemory :: State -> Int -> Interval -> State
updateMemory (State env mem) address value =
    State env (Map.insert address value mem)

transferLoadImm :: State -> Reg -> Integer -> State
transferLoadImm (State env mem) r n =
    State (Map.insert r (evalConstant n) env) mem

evalRegImm :: Environment -> RegImm -> Interval
evalRegImm env operand =
    case operand of
        R r ->
            evalReg env r

        Imm n ->
            evalConstant (fromIntegral n)

transfer :: Trans -> State -> Maybe State
transfer Unconditional state =
    Just state

transfer (Assert cmp r operand) state =
    case checkAssert state cmp r operand of
        DefinitelyFalse -> Nothing
        DefinitelyTrue  -> Just state
        Unknown         -> Just state

transfer (NonCF instr) state =
    case instr of

        LoadImm r imm ->
            Just (transferLoadImm state r (fromIntegral imm))

        Binary _ Add r operand ->
            let oldValue = evalReg (getEnv state) r
                rightValue = evalRegImm (getEnv state) operand
                newValue = addInterval oldValue rightValue
            in Just (updateReg state r newValue)

        Binary _ Sub r operand ->
            let oldValue = evalReg (getEnv state) r
                rightValue = evalRegImm (getEnv state) operand
                newValue = subInterval oldValue rightValue
            in Just (updateReg state r newValue)

        Binary _ Mul r operand ->
            let oldValue = evalReg (getEnv state) r
                rightValue = evalRegImm (getEnv state) operand
                newValue = mulInterval oldValue rightValue
            in Just (updateReg state r newValue)

        Store _ base offset operand ->
            let baseValue = evalReg (getEnv state) base
                offsetValue =
                    case offset of
                        Nothing ->
                            Interval (Finite 0) (Finite 0)

                        Just off ->
                            evalConstant (fromIntegral off)

                address = addInterval baseValue offsetValue
                value = evalRegImm (getEnv state) operand

            in case address of
                Interval (Finite low) (Finite high) ->
                    if low == high
                    then Just (updateMemory state (fromIntegral low) value)
                    else Just state

                _ ->
                    Just state

        Load _ destination base offset ->
            let baseValue = evalReg (getEnv state) base
                offsetValue =
                    case offset of
                        Nothing ->
                            Interval (Finite 0) (Finite 0)

                        Just off ->
                            evalConstant (fromIntegral off)

                address = addInterval baseValue offsetValue
                value = lookupMemory (getMemory state) address

            in Just (updateReg state destination value)

        _ ->
            Just state

analyzeOnce :: CFG -> Map.Map Label State -> Map.Map Label State
analyzeOnce graph states =
    foldl updateNode states (Set.toList nodes)
  where
    nodes =
        Set.fromList
            [ node
            | (source, _, destination) <- Set.toList graph
            , node <- [source, destination]
            ]

    updateNode currentStates node =
        case Map.lookup node currentStates of
            Nothing ->
                let incoming = incomingState graph currentStates node
                in Map.insert node incoming currentStates

            Just _ ->
                currentStates

checkAssert :: State -> Jcmp -> Reg -> RegImm -> Truth
checkAssert state cmp r operand =
    let left = evalReg (getEnv state) r
        right = evalRegImm (getEnv state) operand
    in case cmp of
        Jeq  -> equal left right
        Jne  -> notEqual left right
        Jgt  -> greaterThan left right
        Jge  -> greaterEqual left right
        Jlt  -> lessThan left right
        Jle  -> lessEqual left right
        Jsgt -> greaterThan left right
        Jsge -> greaterEqual left right
        Jslt -> lessThan left right
        Jsle -> lessEqual left right
        Jset -> Unknown
