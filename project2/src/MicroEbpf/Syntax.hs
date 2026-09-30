{-# LANGUAGE PatternSynonyms #-}

-- Defines the micro-eBPF language, meaning the instructions that a program is allowed to contain
-- It handles things like parsing an .asm file into haskell instructions and printing Haskell instructions back to .asm
-- It also defines which instructions are considered valid micro-eBPF instructions.
-- It makes sure we don't accidentally use things outside the language, such as unsupported 64-bit operartions
-- For our project: this tells us what instructions our rewriter can work with and generate

module MicroEbpf.Syntax
  ( pattern Error
  , isMicroEbpf
  , parseProgram
  , parseProgramFile
  , showInstruction
  , showProgram
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, isSpace, toLower)
import Data.List (isSuffixOf)
import Ebpf.Asm
import qualified Ebpf.AsmParser as P
import Numeric (readHex)

-- | The instruction @error@.  The abstract syntax of the starter code has
-- no such instruction; the kit represents @error@ as @Call 0@.
pattern Error :: Instruction
pattern Error = Call 0

-- | Is the instruction one of micro-eBPF (Table 1 of the reference
-- definition)?  The width tag of an arithmetic instruction is irrelevant:
-- all arithmetic is 32-bit (the kit's parser produces 'B32'; the starter
-- code's parser produces 'B64' for unsuffixed instructions).
isMicroEbpf :: Instruction -> Bool
isMicroEbpf i =
  case i of
    Binary bs _ _ _ -> (bs == B32) || (bs == B64)
    Load B32 _ _ _ -> True
    Store B32 _ _ (R _) -> True
    JCond {} -> True
    Jmp _ -> True
    Exit -> True
    Error -> True
    _ -> False

-- | Parse a program from text.
parseProgram :: String -> Either String Program
parseProgram src = do
  normalized <- mapM normalizeLine (zip [1 :: Int ..] (lines src))
  prog <- fmap (map canonical) (P.parse (unlines normalized))
  mapM_ supported (zip [0 :: Int ..] prog)
  Right prog
  where
    supported (a, i) =
      if isMicroEbpf i
        then Right ()
        else Left ("W1: " ++ (showInstruction i ++ (" is not an instruction of micro-eBPF (pc " ++ (show a ++ ")"))))

-- | Parse a program from a file.
parseProgramFile :: FilePath -> IO (Either String Program)
parseProgramFile path = do
  src <- readFile path
  case parseProgram src of
    Left err -> return (Left (path ++ ": " ++ err))
    Right prog -> return (Right prog)

-- | All arithmetic instructions operate on 32-bit values; the width tag of
-- the starter code's abstract syntax is set to 'B32' throughout.
canonical :: Instruction -> Instruction
canonical i =
  case i of
    Binary _ op r ri -> Binary B32 op r ri
    _ -> i

------------------------------------------------------------------------
-- Normalization to the starter-code syntax

normalizeLine :: (Int, String) -> Either String String
normalizeLine (ln, line) =
  let (code, comment) = break (== ';') line
      (ws, rest) = span isSpace code
      (mn, args) = break isSpace rest
      mnl = map toLower mn
      at msg = Left ("line " ++ (show ln ++ (": " ++ msg)))
  in case literalsInRange code of
       Left msg -> at msg
       Right ()
         | mnl == "call" -> at "call is not an instruction of micro-eBPF (the error instruction is written error)"
         | mnl == "error" ->
             if all isSpace args
               then Right (ws ++ ("call 0" ++ comment))
               else at "error takes no operands"
         | "64" `isSuffixOf` mnl ->
             at (mn ++ ": micro-eBPF has no 64-bit instructions (all values have 32 bits; write arithmetic without a suffix, e.g. add r1, r2)")
         | otherwise -> Right (normalizeJump (normalizeMemRefs code) ++ comment)

-- | Every number in a line must have an absolute value below 2^63, so that
-- the starter code's parser, which reads numbers into 64-bit integers,
-- reads it exactly.
literalsInRange :: String -> Either String ()
literalsInRange s =
  case s of
    [] -> Right ()
    c : rest
      | isAlpha c || (c == '_') -> literalsInRange (dropWhile (\x -> isAlphaNum x || (x == '_')) rest)
      | isDigit c ->
          let (tok, rest') = span isAlphaNum s
          in case literalValue tok of
               Just v
                 | v < (2 ^ (63 :: Int)) -> literalsInRange rest'
                 | otherwise -> Left ("number out of range: " ++ tok)
               Nothing -> Left ("invalid number: " ++ tok)
      | otherwise -> literalsInRange rest
  where
    literalValue tok =
      case map toLower tok of
        '0' : 'x' : hs | not (null hs) && all isHexDigit hs ->
          case readHex hs of
            [(v, "")] -> Just (v :: Integer)
            _ -> Nothing
        ds | all isDigit ds -> Just (read ds)
        _ -> Nothing

-- "[r10-8]" and "[r10 - 8]"  ==>  "[r10+-8]"
normalizeMemRefs :: String -> String
normalizeMemRefs s =
  case s of
    [] -> []
    '[' : rest ->
      let (pre, post) = span (\c -> isSpace c || (c == 'r') || isDigit c) rest
          post' = dropWhile isSpace post
      in case post' of
           '-' : more -> '[' : (pre ++ ("+-" ++ normalizeMemRefs (dropWhile isSpace more)))
           _ -> '[' : (pre ++ normalizeMemRefs post)
    c : rest -> c : normalizeMemRefs rest

conditionalMnemonics :: [String]
conditionalMnemonics =
  ["jeq", "jne", "jgt", "jge", "jlt", "jle", "jset", "jsgt", "jsge", "jslt", "jsle"]

-- "jeq r1, 0, -3" ==> "jeq r1, 0, +-3";  "jeq r1, 0, 3" ==> "jeq r1, 0, +3";
-- "ja +-3" ==> "ja -3";  "ja +3" ==> "ja 3"
normalizeJump :: String -> String
normalizeJump code =
  let (ws, rest) = span isSpace code
      (mn, args) = break isSpace rest
      mnl = map toLower mn
  in if mnl `elem` conditionalMnemonics
       then ws ++ (mn ++ lastOperand args)
       else if (mnl == "ja") || (mnl == "jmp")
              then ws ++ (mn ++ jaOperand args)
              else code
  where
    lastOperand args =
      case breakLast ',' args of
        Nothing -> args
        Just (before, after) ->
          let (sp, operand) = span isSpace after
          in case operand of
               '-' : more -> before ++ ("," ++ (sp ++ ("+-" ++ dropWhile isSpace more)))
               d : _ | isDigit d -> before ++ ("," ++ (sp ++ ('+' : operand)))
               _ -> args
    jaOperand args =
      let (sp, operand) = span isSpace args
      in case operand of
           '+' : '-' : _ -> sp ++ drop 1 operand
           '+' : d : _ | isDigit d -> sp ++ drop 1 operand
           _ -> args

breakLast :: Char -> String -> Maybe (String, String)
breakLast c s =
  case [ i | (i, x) <- zip [0 :: Int ..] s, x == c ] of
    [] -> Nothing
    is -> let i = last is in Just (take i s, drop (i + 1) s)

------------------------------------------------------------------------
-- Printing

reg :: Reg -> String
reg (Reg n) = 'r' : show n

regImm :: RegImm -> String
regImm ri =
  case ri of
    R r -> reg r
    Imm i -> show i

memSize :: BSize -> String
memSize bs =
  case bs of
    B8 -> "b"
    B16 -> "h"
    B32 -> "w"
    B64 -> "dw"

width :: BSize -> String
width bs =
  case bs of
    B8 -> "8"
    B16 -> "16"
    B32 -> "32"
    B64 -> "64"

lower :: Show a => a -> String
lower x = map toLower (show x)

memRef :: Reg -> Maybe Offset -> String
memRef r moff =
  case moff of
    Just off | off /= 0 -> "[" ++ (reg r ++ ("+" ++ (show off ++ "]")))
    _ -> "[" ++ (reg r ++ "]")

signedOffset :: Offset -> String
signedOffset off = if off < 0 then show off else '+' : show off

-- | One instruction.  Instructions that are not part of micro-eBPF are
-- printed in the starter-code syntax.
showInstruction :: Instruction -> String
showInstruction i =
  case i of
    Binary _ op r ri -> lower op ++ (" " ++ (reg r ++ (", " ++ regImm ri)))
    Unary bs op r -> lower op ++ ((if bs == B64 then "" else width bs) ++ (" " ++ reg r))
    Store bs r moff (R s) -> "stx" ++ (memSize bs ++ (" " ++ (memRef r moff ++ (", " ++ reg s))))
    Store bs r moff (Imm k) -> "st" ++ (memSize bs ++ (" " ++ (memRef r moff ++ (", " ++ show k))))
    Load bs d s moff -> "ldx" ++ (memSize bs ++ (" " ++ (reg d ++ (", " ++ memRef s moff))))
    LoadImm r k -> "lddw " ++ (reg r ++ (", " ++ show k))
    LoadMapFd r k -> "lmfd " ++ (reg r ++ (", " ++ show k))
    LoadAbs bs k -> "ldabs" ++ (memSize bs ++ (" " ++ show k))
    LoadInd bs r k -> "ldind" ++ (memSize bs ++ (" " ++ (reg r ++ (", " ++ show k))))
    JCond c r ri off -> lower c ++ (" " ++ (reg r ++ (", " ++ (regImm ri ++ (", +" ++ show off)))))
    Jmp off -> "ja " ++ signedOffset off
    Error -> "error"
    Call k -> "call " ++ show k
    Exit -> "exit"

-- | A whole program, one instruction per line.
showProgram :: Program -> String
showProgram prog = unlines (map showInstruction prog)
