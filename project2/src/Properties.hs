-- Properties.hs contains the actual testing logic for checking whether a rewriter is correct
-- It can run the original and rewritten programs from the same initial state and compare their results
-- It checks whether the rewritten program causes an SFI violation and whether it otherwise behaves like the original
-- It also measures things such as instruction/step overhead, which is useful for evaluation the performance of our solution
-- For our project: This is one of the most important files for demonstrating that our rewriter is correct.
module MicroEbpf.Properties
  ( Verdict (..)
  , Failure (..)
  , Summary (..)
  , checkState
  , testRewriting
  , sameFinal
  ) where

import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import Ebpf.Asm (Program)
import MicroEbpf.Contract
import MicroEbpf.Semantics

data Verdict
  = Pass
  | Inconclusive
  | Fail Failure

data Failure
  = Unsound               -- ^ P' ended in violation: (2) fails
  | Mismatch              -- ^ P ended in exit, error or trap; P' did not end the same way: (1) fails
  | Diverged              -- ^ P terminated; P' exhausted its step budget: (1) fails (probably)
  | StuckProgram          -- ^ an instruction that is not part of micro-eBPF was reached

data Summary = Summary
  { sTests :: !Int
  , sPassed :: !Int
  , sInconclusive :: !Int
  , sFailed :: !Int
  , sFailures :: [(InitState, Failure, Outcome, Outcome)]  -- ^ the first (at most 20) failures
  , sExit :: !Int           -- ^ runs of P ending in exit
  , sError :: !Int          -- ^ ... in error
  , sTrap :: !Int           -- ^ ... in trap
  , sViolation :: !Int      -- ^ ... in violation
  , sStepsP :: !Integer     -- ^ steps of P over passed runs
  , sStepsP' :: !Integer    -- ^ steps of P' over passed runs
  , sMaxRatio :: !Double    -- ^ largest steps(P') / steps(P) over passed runs
  }

-- | Do two final configurations agree: same kind, same r0-r10, same memory?
sameFinal :: Outcome -> Outcome -> Bool
sameFinal o o' =
  case (o, o') of
    (Terminated f r m, Terminated f' r' m') ->
      (f == f')
        && (and [ IM.findWithDefault 0 k r == IM.findWithDefault 0 k r' | k <- [0 .. 10] ])
        && (memChanges m == memChanges m')
    _ -> False

-- | Compare P and P' on one initial state.
checkState :: Int -> Int -> Program -> Program -> InitState -> (Verdict, Outcome, Stats, Outcome, Stats)
checkState fuel overhead p p' st =
  let (o, sp) = run fuel p st
      (o', sp') = run (fuel * overhead) p' st
      verdict =
        case (o, o') of
          (_, Terminated FViolation _ _) -> Fail Unsound
          (StuckAt _ _, _) -> Fail StuckProgram
          (_, StuckAt _ _) -> Fail StuckProgram
          (OutOfSteps, _) -> Inconclusive
          (Terminated FViolation _ _, OutOfSteps) -> Inconclusive
          (Terminated FViolation _ _, _) -> Pass
          (Terminated _ _ _, OutOfSteps) -> Fail Diverged
          (Terminated _ _ _, _) -> if sameFinal o o' then Pass else Fail Mismatch
  in (verdict, o, sp, o', sp')

-- | Test a rewriting on a list of initial states.
testRewriting :: Int -> Int -> Program -> Program -> [InitState] -> Summary
testRewriting fuel overhead p p' states =
  foldl' add empty states
  where
    empty = Summary 0 0 0 0 [] 0 0 0 0 0 0 0
    add s st =
      let (v, o, sp, o', sp') = checkState fuel overhead p p' st
          s1 = s { sTests = sTests s + 1 }
          s2 = case o of
                 Terminated FExit _ _ -> s1 { sExit = sExit s1 + 1 }
                 Terminated FError _ _ -> s1 { sError = sError s1 + 1 }
                 Terminated FTrap _ _ -> s1 { sTrap = sTrap s1 + 1 }
                 Terminated FViolation _ _ -> s1 { sViolation = sViolation s1 + 1 }
                 _ -> s1
          ratio = if stSteps sp == 0 then 1 else fromIntegral (stSteps sp') / fromIntegral (stSteps sp)
          counted x = x { sStepsP = sStepsP x + toInteger (stSteps sp)
                        , sStepsP' = sStepsP' x + toInteger (stSteps sp')
                        , sMaxRatio = max (sMaxRatio x) ratio }
      in case v of
           Pass -> counted (s2 { sPassed = sPassed s2 + 1 })
           Inconclusive -> s2 { sInconclusive = sInconclusive s2 + 1 }
           Fail f -> s2 { sFailed = sFailed s2 + 1
                        , sFailures = if sFailed s2 < 20 then sFailures s2 ++ [(st, f, o, o')] else sFailures s2 }
