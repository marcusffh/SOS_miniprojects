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


-- Handle constant assignments
transferSecurity :: Trans -> State -> SecurityState -> SecurityState
transferSecurity Unconditional _ securityState = securityState

transferSecurity (Assert _ _ _) _ securityState = securityState


transferSecurity (NonCF instr) valueState securityState =
    case instr of

        Binary _ Add reg operand ->
            let level =
                    joinLevel
                        (registerLevelOrHigh reg securityState)
                        (operandLevel securityState operand)
            in setRegisterLevel reg level securityState

        Binary _ Sub reg operand ->
            let level =
                    joinLevel
                        (registerLevelOrHigh reg securityState)
                        (operandLevel securityState operand)
            in setRegisterLevel reg level securityState

        Binary _ Mul reg operand ->
            let level =
                    joinLevel
                        (registerLevelOrHigh reg securityState)
                        (operandLevel securityState operand)
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
                level = operandLevel securityState operand
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
                        in setRegisterLevel destination level securityState
                _ -> setRegisterLevel destination High securityState
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

    updateNode currentStates node 
        | node == 0 =
            currentStates
        | otherwise =
            let incoming = 
                    incomingSecurityState graph valueStates currentStates node
            in case Map.lookup node currentStates of 
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

joinIncomingSecurityStates :: [SecurityState] -> SecurityState
joinIncomingSecurityStates [] = emptySecurityState
joinIncomingSecurityStates (state:states) =
    foldl joinSecurityState state states

incomingSecurityState
    :: CFG
    -> Map.Map Label State
    -> Map.Map Label SecurityState
    -> Label
    -> SecurityState
incomingSecurityState graph valueStates securityStates target =
    joinIncomingSecurityStates
        [ transferSecurity trans valueState securityState
        | (source, trans) <- securityPredecessors graph target
        , Just valueState <- [Map.lookup source valueStates]
        , Just securityState <- [Map.lookup source securityStates]
        ]