-- Main.hs is the command-line testing and execution tool supplied with the project 
-- It provides commands such as check, run, test, fuzz, and gen
-- For example, test takes an original program and a rewirteen program and tests wheteher the rewriting satisfies the requirements
-- fuzz automatically generates many random programs, runs our rewriter on them, and tests the resulting programs
-- For our project: we mostly use this file rather than modify it
module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import qualified Data.IntMap.Strict as IM
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Word (Word32, Word64)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

import Ebpf.Asm
import MicroEbpf.Contract
import MicroEbpf.GenProgram (genProgram)
import MicroEbpf.Properties
import MicroEbpf.Semantics
import MicroEbpf.Syntax
import MicroEbpf.WellFormed

usage :: String
usage = unlines
  [ "usage:"
  , "  sfi-kit check [--output] FILE          well-formedness of an input program (r0-r10),"
  , "                                         or of an output program (r0-r15) with --output"
  , "  sfi-kit print FILE                     parse and print"
  , "  sfi-kit run [--trace] [--init FILE | --seed S] FILE"
  , "                                         run once and show the final configuration"
  , "  sfi-kit test [OPTIONS] ORIGINAL REWRITTEN"
  , "                                         test requirements (0)-(2) of a rewriting"
  , "  sfi-kit fuzz [OPTIONS] --rewriter CMD  random programs P through `CMD IN OUT',"
  , "                                         each tested against its output"
  , "  sfi-kit gen [--seed S] [-n N] DIR      write N (default 100) random programs to DIR"
  , ""
  , "OPTIONS:  -n N (initial states per program; test: 1000, fuzz: 200)   --seed S"
  , "          --inputs FILE (additional hand-written initial states)"
  , "          --fuel F (step budget of the original, default 100000)"
  , "          --overhead K (the rewritten program gets K times the budget, default 50)"
  , "          --programs N (fuzz: number of programs, default 200)"
  ]

data Opts = Opts
  { oN :: Maybe Int
  , oSeed :: Word64
  , oFuel :: Int
  , oOverhead :: Int
  , oInputs :: Maybe FilePath
  , oPrograms :: Int
  , oRewriter :: Maybe String
  , oTrace :: Bool
  , oInit :: Maybe FilePath
  , oOutput :: Bool
  , oFiles :: [String]
  }

defaults :: Opts
defaults = Opts Nothing 1 100000 50 Nothing 200 Nothing False Nothing False []

parseOpts :: [String] -> Either String Opts
parseOpts = go defaults
  where
    go o args =
      case args of
        [] -> Right o { oFiles = reverse (oFiles o) }
        "-n" : v : rest -> num v >>= \k -> go (o { oN = Just k }) rest
        "--seed" : v : rest -> num v >>= \k -> go (o { oSeed = fromInteger k }) rest
        "--fuel" : v : rest -> num v >>= \k -> go (o { oFuel = k }) rest
        "--overhead" : v : rest -> num v >>= \k -> go (o { oOverhead = k }) rest
        "--inputs" : v : rest -> go (o { oInputs = Just v }) rest
        "--programs" : v : rest -> num v >>= \k -> go (o { oPrograms = k }) rest
        "--rewriter" : v : rest -> go (o { oRewriter = Just v }) rest
        "--trace" : rest -> go (o { oTrace = True }) rest
        "--init" : v : rest -> go (o { oInit = Just v }) rest
        "--output" : rest -> go (o { oOutput = True }) rest
        flag : _ | "-" `isPrefixOf` flag -> Left ("unknown option " ++ flag)
        file : rest -> go (o { oFiles = file : oFiles o }) rest
    num :: (Read a) => String -> Either String a
    num v = case reads v of
              [(k, "")] -> Right k
              _ -> Left ("not a number: " ++ v)

main :: IO ()
main = do
  args <- getArgs
  case args of
    cmd : rest ->
      case parseOpts rest of
        Left err -> die err
        Right o -> dispatch cmd o
    [] -> die "no command"

-- | "1 instruction", "2 instructions", ...
instructions :: Int -> String
instructions n = show n ++ (if n == 1 then " instruction" else " instructions")

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  hPutStrLn stderr usage
  exitFailure

load :: FilePath -> IO Program
load path = do
  r <- parseProgramFile path
  case r of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right p -> return p

dispatch :: String -> Opts -> IO ()
dispatch cmd o =
  case (cmd, oFiles o) of
    ("check", [f]) -> do
      p <- load f
      let d = if oOutput o then Output else Input
      case wellFormed d p of
        Right () -> do
          printf "%s: well-formed %s program (registers r0-r%d), %s\n"
            f (if d == Input then "input" else "output") (registerCount d - 1) (instructions (length p))
          forM_ (jumpsOutside p) (\(a, t) ->
            printf "  note: the jump at pc %d goes to address %d, outside the code region [0, %d)\n" a t (length p))
        Left err -> do
          printf "%s: NOT well-formed: %s\n" f err
          exitWith (ExitFailure 1)
    ("print", [f]) -> do
      p <- load f
      putStr (showProgram p)
    ("run", [f]) -> doRun o f
    ("test", [f, f']) -> do
      ok <- doTest o f f'
      unless ok (exitWith (ExitFailure 1))
    ("fuzz", []) -> doFuzz o
    ("gen", [dir]) -> doGen o dir
    _ -> die ("bad command line for " ++ cmd)

------------------------------------------------------------------------

readInits :: FilePath -> IO [InitState]
readInits path = do
  src <- readFile path
  case parseInits src of
    Left err -> die (path ++ ": " ++ err)
    Right sts -> do
      forM_ (zip [1 :: Int ..] sts) (\(k, st) ->
        case validInit st of
          Right () -> return ()
          Left err -> die (path ++ ": initial state " ++ (show k ++ (": " ++ err))))
      return sts

givenStates :: Opts -> IO [InitState]
givenStates o =
  case oInputs o of
    Nothing -> return []
    Just path -> readInits path

randomStates :: Word64 -> Int -> Program -> [InitState]
randomStates seed count p = runG seed (mapM (const (genInit p)) [1 .. count])

doRun :: Opts -> FilePath -> IO ()
doRun o f = do
  p <- load f
  case wellFormed Output p of
    Left err -> die (f ++ ": NOT well-formed: " ++ err)
    Right () -> return ()
  st <- case oInit o of
          Just path -> do
            sts <- readInits path
            case sts of
              st : _ -> return st
              [] -> die "no initial state in file"
          Nothing -> return (runG (oSeed o) (genInit p))
  let (out, stats, tr, why) = runTrace 60 (oFuel o) p st
      usesMonitor = any (\i -> any (\(Reg r) -> r > 10) (registersOf i)) p
  putStr (renderInit st)
  when (oTrace o) (do
    putStrLn "-- last executed instructions (pc: instruction   registers before)"
    forM_ tr (\(TraceEntry pc i regs) ->
      printf "%4d: %-28s %s\n" pc (showInstruction i) (showRegs i regs)))
  printf "-- %s\n" why
  putStr (describeOutcome usesMonitor st out)
  printf "-- %d steps, %d loads, %d stores\n" (stSteps stats) (stLoads stats) (stStores stats)

showRegs :: Instruction -> IM.IntMap Word32 -> String
showRegs i regs =
  unwords [ 'r' : (show r ++ ("=0x" ++ showHex (IM.findWithDefault 0 r regs) "")) | Reg r <- registersOf i ]

finalName :: Final -> String
finalName f =
  case f of
    FExit -> "exit"
    FError -> "error"
    FTrap -> "trap"
    FViolation -> "violation"

-- | The final configuration: its kind, the registers and the words whose
-- value differs from the initial memory.
describeOutcome :: Bool -> InitState -> Outcome -> String
describeOutcome allRegs st out =
  case out of
    Terminated f regs mem ->
      let initial x = M.findWithDefault (fillWord (iFill st) x) x (iWords st)
          changed = [ (x, v) | (x, v) <- memChanges mem ++ reverted mem, v /= initial x ]
          -- words of the initial state that were overwritten with the fill value
          reverted m = [ (x, v) | (x, _) <- M.toList (iWords st), let v = memRead m x, v == fillWord (iFill st) x ]
      in unlines
           ( [ "-- final configuration: " ++ finalName f
             , "-- registers: " ++ unwords [ 'r' : (show r ++ ("=0x" ++ showHex (IM.findWithDefault 0 r regs) "")) | r <- [0 .. (if allRegs then 15 else 10)] ]
             ]
             ++ (case changed of
                   [] -> ["-- memory: unchanged"]
                   ch -> ("-- memory words changed by the run (" ++ (show (length ch) ++ "):"))
                           : [ "--   0x" ++ (showHex x (": 0x" ++ showHex v "")) | (x, v) <- take 16 ch ]
                           ++ [ "--   ..." | length ch > 16 ])
           )
    OutOfSteps -> "-- no final configuration: the step budget is exhausted\n"
    StuckAt pc why -> "-- stuck at pc " ++ (show pc ++ (": " ++ (why ++ "\n")))

shortOutcome :: Outcome -> String
shortOutcome out =
  case out of
    Terminated f regs _ -> finalName f ++ (" (r0 = 0x" ++ (showHex (IM.findWithDefault 0 0 regs) ")"))
    OutOfSteps -> "out of steps"
    StuckAt pc why -> "stuck at pc " ++ (show pc ++ (": " ++ why))

doTest :: Opts -> FilePath -> FilePath -> IO Bool
doTest o f f' = do
  p <- load f
  p' <- load f'
  wf1 <- report "original " f Input p
  wf2 <- report "rewritten" f' Output p'
  if not (wf1 && wf2)
    then return False
    else do
      given <- givenStates o
      let sts = given ++ randomStates (oSeed o) (maybe 1000 id (oN o)) p
          s = testRewriting (oFuel o) (oOverhead o) p p' sts
      printSummary o p p' s
      return (sFailed s == 0)
  where
    report :: String -> FilePath -> Dialect -> Program -> IO Bool
    report what path d prog =
      case wellFormed d prog of
        Right () -> do
          printf "%s: %s  (%s, well-formed %s program)\n" what path (instructions (length prog))
            (if d == Input then "input" else "output")
          return True
        Left err -> do
          printf "%s: %s  NOT WELL-FORMED (%s program): %s\n" what path
            (if d == Input then "input" else "output") err
          return False

printSummary :: Opts -> Program -> Program -> Summary -> IO ()
printSummary o p p' s = do
  printf "%d initial states, step budget %d (x%d for the rewritten program)\n"
    (sTests s) (oFuel o) (oOverhead o)
  printf "  passed %d, inconclusive %d, FAILED %d\n" (sPassed s) (sInconclusive s) (sFailed s)
  printf "  the original ended in exit %d, error %d, trap %d, violation %d times\n"
    (sExit s) (sError s) (sTrap s) (sViolation s)
  let static = (fromIntegral (length p') :: Double) / fromIntegral (length p)
      dyn = if sStepsP s == 0 then 1 else (fromIntegral (sStepsP' s) :: Double) / fromIntegral (sStepsP s)
  printf "  overhead: %.2fx instructions (static); %.2fx steps on average, at most %.2fx (dynamic)\n"
    static dyn (sMaxRatio s)
  forM_ (zip [1 :: Int ..] (take 3 (sFailures s))) (\(k, (st, fl, out, out')) -> do
    printf "--- failure %d: %s\n" k (describe fl)
    printf "    original: %s; rewritten: %s\n" (shortOutcome out) (shortOutcome out')
    putStr (renderInit st))
  where
    describe fl =
      case fl of
        Unsound -> "the rewritten program ends in violation: requirement (2) fails"
        Mismatch -> "the rewritten program does not end like the original (kind, r0-r10 or memory): requirement (1) fails"
        Diverged -> "the rewritten program exhausted its step budget although the original terminated: requirement (1) fails"
        StuckProgram -> "a program reached an instruction that is not part of micro-eBPF"

doFuzz :: Opts -> IO ()
doFuzz o =
  case oRewriter o of
    Nothing -> die "fuzz needs --rewriter CMD"
    Just cmd -> do
      let dir = "fuzz-out"
      createDirectoryIfMissing True dir
      given <- givenStates o
      results <- forM [1 .. oPrograms o] (\k -> do
        let p = runG (oSeed o + fromIntegral k) genProgram
            fin = dir </> ("prog" ++ (show k ++ ".asm"))
            fout = dir </> ("prog" ++ (show k ++ ".sfi.asm"))
        writeFile fin (showProgram p)
        let (prog, cmdArgs) = case words cmd of
                                w : ws -> (w, ws)
                                [] -> ("true", [])
        (code, _, err) <- readProcessWithExitCode prog (cmdArgs ++ [fin, fout]) ""
        case code of
          ExitFailure _ -> do
            printf "program %d (%s): the rewriter failed on a well-formed program: requirement (0) fails: %s\n" k fin (take 200 err)
            return False
          ExitSuccess -> do
            r <- parseProgramFile fout
            case r of
              Left e -> do
                printf "program %d: cannot parse the rewriter's output: %s\n" k e
                return False
              Right p' ->
                case wellFormed Output p' of
                  Left e -> do
                    printf "program %d (%s): the output is not well-formed: requirement (0) fails: %s\n" k fin e
                    return False
                  Right () -> do
                    let sts = given ++ randomStates ((oSeed o * 7919) + fromIntegral k) (maybe 200 id (oN o)) p
                        s = testRewriting (oFuel o) (oOverhead o) p p' sts
                    if sFailed s == 0
                      then return True
                      else do
                        printf "program %d (%s): %d of %d initial states FAIL\n" k fin (sFailed s) (sTests s)
                        printSummary o p p' s
                        return False)
      let failed = length (filter not results)
      printf "fuzz: %d programs, %d failed (programs are kept in %s)\n" (length results) failed dir
      when (failed > 0) (exitWith (ExitFailure 1))

doGen :: Opts -> FilePath -> IO ()
doGen o dir = do
  createDirectoryIfMissing True dir
  let count = maybe 100 id (oN o)
  forM_ [1 .. count] (\k -> do
    let p = runG (oSeed o + fromIntegral k) genProgram
    writeFile (dir </> ("prog" ++ (show k ++ ".asm"))) (showProgram p))
  printf "wrote %d programs to %s\n" count dir
