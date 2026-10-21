module InformationFlow where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Ebpf.Asm

import Security
import SecurityState
import CFG
import Environment
import Interval
import Expressions

branchCondition :: CFG -> Label -> Maybe (Jcmp, Reg, RegImm)
branchCondition graph branch =
    case
        [ (cmp, reg, operand)
        | (source, Assert cmp reg operand, _) <- Set.toList graph
        , source == branch
        ]
    of
        condition:_ -> Just condition
        []          -> Nothing

controlContext
    :: CFG
    -> Label
    -> Map.Map Label SecurityState
    -> Map.Map Label (Set.Set Label)
    -> SecurityLevel
controlContext graph target securityStates controlDeps =
    joinLevels
        [ branchLevel cmp reg operand state
        | (branch, controlledNodes) <- Map.toList controlDeps
        , Set.member target controlledNodes
        , Just state <- [Map.lookup branch securityStates]
        , Just (cmp, reg, operand) <- [branchCondition graph branch]
        ]

initialSecurityStates :: SecurityState -> Map.Map Label SecurityState
initialSecurityStates initialState =
    Map.singleton 0 initialState

-- Look up security levels
registerLevelOrHigh :: Reg -> SecurityState -> SecurityLevel
registerLevelOrHigh reg state =
    case getRegisterLevel reg state of
        Just level -> level
        Nothing    -> High

operandLevel :: SecurityState -> RegImm -> SecurityLevel
operandLevel state operand =
    case operand of
        R reg -> registerLevelOrHigh reg state
        Imm _ -> Low

branchLevel :: Jcmp -> Reg -> RegImm -> SecurityState -> SecurityLevel
branchLevel _ reg operand state =
    joinLevel
        (registerLevelOrHigh reg state)
        (operandLevel state operand)

-- Handle constant assignments
transferSecurity 
    :: SecurityLevel -> Trans -> State -> SecurityState -> SecurityState
transferSecurity _ Unconditional _ securityState = 
    securityState

transferSecurity _ (Assert _ _ _) _ securityState = 
    securityState


transferSecurity context (NonCF instr) valueState securityState =
    case instr of
    
        LoadImm reg _ ->
            setRegisterLevel reg
                (joinLevel context Low)
                securityState
        
        Binary _ Mov reg operand ->
            setRegisterLevel reg 
                (joinLevel context (operandLevel securityState operand))
                securityState
        
        
        Binary _ Add reg operand ->
            let level =
                    joinLevels
                        [ context
                        , registerLevelOrHigh reg securityState
                        , operandLevel securityState operand
                        ]
            in setRegisterLevel reg level securityState
        
        Binary _ Sub reg operand ->
            let level =
                    joinLevels
                        [ context
                        , registerLevelOrHigh reg securityState
                        , operandLevel securityState operand
                        ]
            in setRegisterLevel reg level securityState

        Binary _ Mul reg operand ->
            let level =
                    joinLevels
                        [ context
                        , registerLevelOrHigh reg securityState
                        , operandLevel securityState operand
                        ]
            in setRegisterLevel reg level securityState
        


        Store _ base offset operand ->
            let baseValue =
                    case valueState of
                        State env _ -> evalReg env base
                offsetValue =
                    case offset of
                        Nothing -> Interval (Finite 0) (Finite 0)
                        Just off -> evalConstant (fromIntegral off)
                address = addInterval baseValue offsetValue
                level = 
                    joinLevel context (operandLevel securityState operand)
            in case address of
                Interval (Finite low) (Finite high)
                    | low == high ->
                        setMemoryLevel (fromIntegral low) level securityState
                _ -> securityState
        
        Load _ destination base offset ->
            let baseValue =
                    case valueState of
                        State env _ -> evalReg env base
                offsetValue =
                    case offset of
                        Nothing -> Interval (Finite 0) (Finite 0)
                        Just off -> evalConstant (fromIntegral off)
                address = addInterval baseValue offsetValue
            in case address of
                Interval (Finite low) (Finite high)
                    | low == high ->
                        let level =
                                case getMemoryLevel (fromIntegral low) securityState of
                                    Just l  -> l
                                    Nothing -> High
                        in setRegisterLevel destination 
                            (joinLevel context level)
                            securityState
                _ -> setRegisterLevel destination
                        (joinLevel context High)
                        securityState
        _ -> securityState

analyzeSecurityOnce
    :: CFG
    -> Map.Map Label State
    -> Map.Map Label SecurityState
    -> Map.Map Label SecurityState
analyzeSecurityOnce graph valueStates securityStates =
    foldl updateNode securityStates nodes
  where
    nodes =
        Set.toList $
            Set.fromList
                [ node
                | (source, _, destination) <- Set.toList graph
                , node <- [source, destination]
                ]
    exits =
        Set.difference
            (Set.fromList nodes)
            (Set.fromList
                [ source
                | (source, _, _) <- Set.toList graph
                ])

    postDom = postDominators graph exits

    controlDeps = controlDependencies graph postDom
    
    updateNode currentStates node 
        | node == 0 =
            currentStates
        | otherwise =
            case incomingSecurityState
                         graph valueStates currentStates controlDeps node of 
                Nothing -> 
                    currentStates

                Just incoming ->
                    case Map.lookup node currentStates of 
                        Nothing -> 
                            Map.insert node incoming currentStates

                        Just oldState ->
                            Map.insert node
                            (joinSecurityState oldState incoming)
                            currentStates

analyzeSecurity
    :: CFG
    -> Map.Map Label State
    -> Map.Map Label SecurityState
    -> Map.Map Label SecurityState
analyzeSecurity graph valueStates securityStates =
    let newStates =
            analyzeSecurityOnce graph valueStates securityStates
    in if newStates == securityStates
       then securityStates
       else analyzeSecurity graph valueStates newStates

securityPredecessors :: CFG -> Label -> [(Label, Trans)]
securityPredecessors graph target =
    [ (source, trans)
    | (source, trans, destination) <- Set.toList graph
    , destination == target
    ]

joinIncomingSecurityStates :: [SecurityState] ->  Maybe SecurityState
joinIncomingSecurityStates [] = Nothing
joinIncomingSecurityStates (state:states) =
    Just (foldl joinSecurityState state states)

incomingSecurityState
    :: CFG
    -> Map.Map Label State
    -> Map.Map Label SecurityState
    -> Map.Map Label (Set.Set Label)
    -> Label
    -> Maybe SecurityState
incomingSecurityState graph valueStates securityStates controlDeps target =
        joinIncomingSecurityStates
        [ transferSecurity
            (controlContext graph source securityStates controlDeps)
            trans
            valueState
            securityState
        | (source, trans) <- securityPredecessors graph target
        , Just valueState <- [Map.lookup source valueStates]
        , Just securityState <- [Map.lookup source securityStates]
        ]
