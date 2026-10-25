
module Main where

import qualified Data.Map as Map
import System.Exit (exitFailure)

import Ebpf.Asm
import CFG
import Analysis
import Environment
import InformationFlow
import Security
import SecurityState

runAnalysis :: Program -> SecurityState -> Map.Map Label SecurityState
runAnalysis program initialState =
    let graph = cfg program
        initialValueState = State Map.empty Map.empty
        valueStates =
            analyze 0 graph (Map.singleton 0 initialValueState)
        initialStates = initialSecurityStates initialState
    in analyzeSecurity graph valueStates initialStates

checkRegister
    :: String
    -> SecurityLevel
    -> Label
    -> Reg
    -> Map.Map Label SecurityState
    -> IO Bool
checkRegister testName expected label reg states =
    case Map.lookup label states of
        Nothing -> do
            putStrLn ("FAIL: " ++ testName ++ " - no state at label "
                      ++ show label)
            pure False

        Just state ->
            let actual = getRegisterLevel reg state
            in if actual == Just expected
               then do
                   putStrLn ("PASS: " ++ testName)
                   pure True
               else do
                   putStrLn ("FAIL: " ++ testName
                       ++ " - expected " ++ show expected
                       ++ ", got " ++ show actual)
                   pure False

-- A secret branch controls the assignment to r0.
-- At the merge point (label 3), r0 should be High.
secretBranchTest :: IO Bool
secretBranchTest =
    let program =
            [ Binary B64 Mov (Reg 0) (Imm 0)
            , JCond Jeq (Reg 1) (Imm 0) 1
            , Binary B64 Mov (Reg 0) (Imm 1)
            , Exit
            ]
        initialState =
            setRegisterLevel (Reg 1) High emptySecurityState
        states = runAnalysis program initialState
    in checkRegister "secret branch raises assigned register to High"
           High 3 (Reg 0) states

-- A public branch should not introduce a High control dependency.
publicBranchTest :: IO Bool
publicBranchTest =
    let program =
            [ Binary B64 Mov (Reg 0) (Imm 0)
            , JCond Jeq (Reg 1) (Imm 0) 1
            , Binary B64 Mov (Reg 0) (Imm 1)
            , Exit
            ]
        initialState =
            setRegisterLevel (Reg 1) Low emptySecurityState
        states = runAnalysis program initialState
    in checkRegister "public branch keeps assigned register Low"
           Low 3 (Reg 0) states

nestedBranchTest :: IO Bool
nestedBranchTest =
    let program =
            [ Binary B64 Mov (Reg 0) (Imm 0)
            , JCond Jeq (Reg 1) (Imm 0) 2
            , JCond Jeq (Reg 2) (Imm 0) 1
            , Binary B64 Mov (Reg 0) (Imm 1)
            , Exit
            ]
        initialState =
            setRegisterLevel (Reg 1) High
                (setRegisterLevel (Reg 2) Low emptySecurityState)
        states = runAnalysis program initialState
    in checkRegister "nested branch under secret condition raises r0 to High"
           High 4 (Reg 0) states

directRegisterFlowTest :: IO Bool
directRegisterFlowTest =
    let program :: Program
        program =
            [ Binary B64 Mov (Reg 0) (R (Reg 1))
            , Exit
            ]
        initialState =
            setRegisterLevel (Reg 1) High emptySecurityState
        states = runAnalysis program initialState
    in checkRegister "copying a High register propagates High"
           High 1 (Reg 0) states

highLoadTest :: IO Bool
highLoadTest =
    let program =
            [ Binary B64 Mov (Reg 2) (Imm 100)
            , Store B64 (Reg 2) (Just 0) (R (Reg 1))
            , Load B64 (Reg 3) (Reg 2) (Just 0)
            , Exit
            ]
        initialState =
            setRegisterLevel (Reg 1) High emptySecurityState
        states = runAnalysis program initialState
    in checkRegister "loading a High memory value taints destination"
           High 3 (Reg 3) states

highStoreTest :: IO Bool
highStoreTest =
    let program =
            [ Binary B64 Mov (Reg 2) (Imm 100)
            , Store B64 (Reg 2) (Just 0) (R (Reg 1))
            , Exit
            ]
        initialState =
            setRegisterLevel (Reg 1) High emptySecurityState
        states = runAnalysis program initialState
    in case Map.lookup 2 states of
        Nothing -> do
            putStrLn "FAIL: High store - no state at label 2"
            pure False
        Just state ->
            let actual = getMemoryLevel 100 state
            in if actual == Just High
               then do
                   putStrLn "PASS: storing a High value taints memory"
                   pure True
               else do
                   putStrLn ("FAIL: storing a High value taints memory"
                       ++ " - expected Just High, got " ++ show actual)
                   pure False

lowOverwriteTest :: IO Bool
lowOverwriteTest =
    let program =
            [ Binary B64 Mov (Reg 2) (Imm 100)
            , Store B64 (Reg 2) (Just 0) (R (Reg 1))
            , Store B64 (Reg 2) (Just 0) (Imm 42)
            , Exit
            ]
        initialState =
            setRegisterLevel (Reg 1) High emptySecurityState
        states = runAnalysis program initialState
    in case Map.lookup 3 states of
        Nothing -> do
            putStrLn "FAIL: Low overwrite - no state at label 3"
            pure False
        Just state ->
            let actual = getMemoryLevel 100 state
            in if actual == Just Low
               then do
                   putStrLn "PASS: Low store overwrites High memory"
                   pure True
               else do
                   putStrLn ("FAIL: Low store overwrites High memory"
                       ++ " - expected Just Low, got " ++ show actual)
                   pure False


unknownAddressStoreTest :: IO Bool
unknownAddressStoreTest =
    let program =
            [ Binary B64 Mov (Reg 2) (Imm 100)
            , Store B64 (Reg 2) (Just 0) (Imm 42)
            , Store B64 (Reg 3) (Just 0) (Imm 99)
            , Exit
            ]
        initialState =
            setRegisterLevel (Reg 3) Low emptySecurityState
        states = runAnalysis program initialState
    in case Map.lookup 3 states of
        Nothing -> do
            putStrLn "FAIL: Unknown-address store - no state at label 3"
            pure False
        Just state ->
            let actual = getMemoryLevel 100 state
            in if actual == Just High
               then do
                   putStrLn "PASS: uncertain store conservatively taints memory"
                   pure True
               else do
                   putStrLn ("FAIL: uncertain store conservatively taints memory"
                       ++ " - expected Just High, got " ++ show actual)
                   pure False

uninitializedMemoryTest :: IO Bool
uninitializedMemoryTest =
    let program =
            [ Binary B64 Mov (Reg 2) (Imm 100)
            , Load B64 (Reg 3) (Reg 2) (Just 0)
            , Exit
            ]
        states = runAnalysis program emptySecurityState
    in checkRegister "loading uninitialized memory defaults to High"
           High 2 (Reg 3) states


branchInfluenceEndsTest :: IO Bool
branchInfluenceEndsTest =
    let program =
            [ Binary B64 Mov (Reg 0) (Imm 0)
            , JCond Jeq (Reg 1) (Imm 0) 1
            , Binary B64 Mov (Reg 0) (Imm 1)
            , Binary B64 Mov (Reg 2) (Imm 42)
            , Exit
            ]
        initialState =
            setRegisterLevel (Reg 1) High emptySecurityState
        states = runAnalysis program initialState
    in checkRegister "secret branch influence ends after reconvergence"
           Low 4 (Reg 2) states

main :: IO ()
main = do
    results <- sequence
        [ secretBranchTest
        , publicBranchTest
        , nestedBranchTest
        , directRegisterFlowTest
        , highLoadTest
        , highStoreTest
        , lowOverwriteTest
        , unknownAddressStoreTest
        , uninitializedMemoryTest
        , branchInfluenceEndsTest
        ]
    if and results
        then putStrLn "\nAll information-flow tests passed."
        else exitFailure
