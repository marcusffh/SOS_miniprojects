-- Contract.hs defines the environment in which the program is allowed to run
-- Most importantly, it defines the data region as [DB, DL] where DB is the beginning and DL is the end
-- It also specifies the inital values of important registers: r1 = DB, r2 = DL-DB, and r10 = DL
-- It provides functions for creating and checking initial states, including the inital registers and memory contents
-- For our project: This tells us what the rewriter's memory checks need to protect - accesses must stay inside [DB, DL]
module MicroEbpf.Contract
  ( Region (..)
  , inRegion
  , Fill (..)
  , fillWord
  , InitState (..)
  , initRegisters
  , validInit
  , renderInit
  , parseInits
  , G
  , runG
  , genInit
  , genMaskingInit
  , programConstants
  ) where

import Control.Monad (forM, replicateM, unless, when)
import Data.Bits (shiftR, xor)
import Data.Char (isSpace)
import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Word (Word32, Word64)
import Numeric (readHex, showHex)
import Ebpf.Asm
import MicroEbpf.GenUtil

------------------------------------------------------------------------
-- Data region, memory fill, initial states

-- | The data region [DB, DL).
data Region = Region
  { rBase :: Word32    -- ^ DB, the initial value of r1
  , rLimit :: Word32   -- ^ DL, the initial value of r10
  }
  deriving (Eq, Show)

-- | Does the address lie in the data region?
inRegion :: Region -> Word32 -> Bool
inRegion r x = (rBase r <= x) && (x < rLimit r)

-- | The words of the initial memory that are not given explicitly.
data Fill
  = FillZero
  | FillPattern Word64   -- ^ a pseudo-random word for every address
  deriving (Eq, Show)

fillWord :: Fill -> Word32 -> Word32
fillWord f x =
  case f of
    FillZero -> 0
    FillPattern s -> fromIntegral (mix (s + (fromIntegral x * 0x9e3779b97f4a7c15)))
  where
    mix z0 =
      let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
          z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
      in z2 `xor` (z2 `shiftR` 31)

data InitState = InitState
  { iRegion :: Region
  , iRegs :: M.Map Int Word32      -- ^ r0, r3-r9, r11-r15 (missing = 0)
  , iFill :: Fill
  , iWords :: M.Map Word32 Word32  -- ^ initial words that differ from the fill
  }
  deriving (Eq, Show)

-- | All sixteen registers of the initial state (a program that names only
-- r0-r10 does not see r11-r15).
initRegisters :: InitState -> M.Map Int Word32
initRegisters st =
  let r = iRegion st
      others = M.fromList [ (k, M.findWithDefault 0 k (iRegs st)) | k <- [0 .. 15] ]
  in M.insert 1 (rBase r) (M.insert 2 (rLimit r - rBase r) (M.insert 10 (rLimit r) others))

-- | Check an initial state against the contract.
validInit :: InitState -> Either String ()
validInit st = do
  let r = iRegion st
      db = toInteger (rBase r)
      dl = toInteger (rLimit r)
  unless ((db `mod` 4) == 0) (Left "contract: DB is not a multiple of 4")
  unless ((dl `mod` 4) == 0) (Left "contract: DL is not a multiple of 4")
  unless (db < dl) (Left "contract: DB < DL does not hold")
  unless ((dl - db) >= 512) (Left "contract: DL - DB >= 512 does not hold")
  mapM_ (\x -> unless (((toInteger x `mod` 4) == 0) && inRegion r x)
                 (Left ("contract: memory word at 0x" ++ (showHex x " is not an aligned address of the data region"))))
        (M.keys (iWords st))
  mapM_ (\k -> unless ((k `notElem` [1, 2, 10]) && (0 <= k) && (k <= 15))
                 (Left ("contract: cannot set register r" ++ show k)))
        (M.keys (iRegs st))

------------------------------------------------------------------------
-- Text format
--
--   base 0x20000          -- DB (r1); r2 = DL - DB
--   limit 0x20800         -- DL (r10)
--   fill zero             -- or: fill pattern 0x1234 (the other words)
--   mem 0x20000 0x11 0x22 -- words at 0x20000, 0x20004, ...
--   r3 0x2a               -- r0, r3-r9, r11-r15 (default 0)
--   ---                   -- separates initial states

-- | Render an initial state in the text format.
renderInit :: InitState -> String
renderInit st =
  unlines
    ( [ "base 0x" ++ showHex (rBase r) ""
      , "limit 0x" ++ showHex (rLimit r) ""
      , case iFill st of
          FillZero -> "fill zero"
          FillPattern s -> "fill pattern 0x" ++ showHex s ""
      ]
      ++ [ "mem 0x" ++ (showHex x "" ++ concatMap (\v -> " 0x" ++ showHex v "") vs) | (x, vs) <- runs (M.toList (iWords st)) ]
      ++ [ 'r' : (show k ++ (" 0x" ++ showHex v "")) | (k, v) <- M.toList (iRegs st), v /= 0 ]
    )
  where
    r = iRegion st
    -- consecutive words on one line
    runs ws =
      case ws of
        [] -> []
        (x, v) : rest ->
          let (more, rest') = spanRun (x + 4) rest
          in (x, v : more) : runs rest'
    spanRun next ws =
      case ws of
        (y, v) : rest | y == next -> let (more, rest') = spanRun (y + 4) rest in (v : more, rest')
        _ -> ([], ws)

-- | Parse one or more initial states separated by lines "---".  A register
-- value or memory word is a number in [-2^31, 2^32); a negative number
-- stands for its two's complement (-4 is 0xfffffffc).
parseInits :: String -> Either String [InitState]
parseInits src = mapM parseOne (filter (not . all blank) (splitOn (lines src)))
  where
    blank line = all isSpace (takeWhile (/= '#') line)
    splitOn ls =
      case break isSep ls of
        (chunk, []) -> [chunk]
        (chunk, _ : rest) -> chunk : splitOn rest
    isSep line = takeWhile (not . isSpace) (dropWhile isSpace line) == "---"

    parseOne ls = do
      kvs <- mapM field (filter (not . null . snd) [ (ln, words (takeWhile (/= '#') ln)) | ln <- ls ])
      let get k = lookup k kvs
      db <- maybe (Left "initial state without 'base'") (\v -> single v >>= address) (get "base")
      dl <- maybe (Left "initial state without 'limit'") (\v -> single v >>= address) (get "limit")
      fill <- case get "fill" of
                Nothing -> Right FillZero
                Just ["zero"] -> Right FillZero
                Just ["pattern", s] -> fmap (FillPattern . fromInteger) (num s)
                Just _ -> Left "fill must be 'fill zero' or 'fill pattern N'"
      mems <- forM [ v | (k, v) <- kvs, k == "mem" ] (\v ->
                case v of
                  a : ws -> do
                    x <- address a
                    vs <- mapM wordValue ws
                    Right (zip [ x + (4 * fromIntegral i) | i <- [0 :: Int ..] ] vs)
                  [] -> Left "mem needs an address")
      regs <- forM [ (k, v) | (k, v) <- kvs, isReg k ] (\(k, v) -> do
                x <- single v >>= wordValue
                r <- regNum k
                Right (r, x))
      Right InitState
        { iRegion = Region db dl
        , iRegs = M.fromList regs
        , iFill = fill
        , iWords = M.fromList (concat mems)
        }

    field (_, w : ws) = Right (w, ws)
    field (ln, []) = Left ("bad line: " ++ ln)
    single ws = case ws of
                  [w] -> Right w
                  _ -> Left ("expected one value, got: " ++ unwords ws)
    isReg k = case k of
                'r' : ds -> not (null ds) && all (`elem` "0123456789") ds
                _ -> False
    regNum k = do
      let r = read (drop 1 k) :: Int
      unless ((r >= 0) && (r <= 15) && (r `notElem` [1, 2, 10]))
        (Left ("cannot set register " ++ (k ++ " (r1, r2 and r10 are given by base and limit)")))
      Right r
    address w = do
      x <- num w
      unless ((0 <= x) && (x < (2 ^ (32 :: Int)))) (Left ("address out of range: " ++ w))
      Right (fromInteger x)
    wordValue w = do
      x <- num w
      unless ((negate (2 ^ (31 :: Int)) <= x) && (x < (2 ^ (32 :: Int))))
        (Left ("value out of range [-2^31, 2^32): " ++ w))
      Right (fromInteger (x `mod` (2 ^ (32 :: Int))))
    num w = case w of
              '0' : x : hs | x `elem` "xX" -> case readHex hs of
                                              [(v, "")] -> Right v
                                              _ -> Left ("bad number " ++ w)
              '-' : ds | not (null ds) && all (`elem` "0123456789") ds -> Right (negate (read ds))
              ds | not (null ds) && all (`elem` "0123456789") ds -> Right (read ds)
              _ -> Left ("bad number " ++ w)

------------------------------------------------------------------------
-- Program-directed generation

-- | Constants occurring in a program (immediates and memory offsets); the
-- generator uses them, and their neighbors, as "interesting" values.
programConstants :: Program -> [Integer]
programConstants prog = nub (sort (concatMap consts prog))
  where
    consts i =
      case i of
        Binary _ _ _ (Imm k) -> [toInteger k]
        Store _ _ moff _ -> maybe [] (\o -> [toInteger o]) moff
        Load _ _ _ moff -> maybe [] (\o -> [toInteger o]) moff
        JCond _ _ (Imm k) _ -> [toInteger k]
        _ -> []

-- | The memory offsets of the loads and stores of a program.
accessOffsets :: Program -> [Integer]
accessOffsets = nub . mapMaybe site
  where
    site i =
      case i of
        Load _ _ _ moff -> Just (maybe 0 toInteger moff)
        Store _ _ moff _ -> Just (maybe 0 toInteger moff)
        _ -> Nothing

alignDown :: Integer -> Integer
alignDown x = (x `div` 4) * 4

wordMod :: Integer -> Word32
wordMod v = fromInteger (v `mod` (2 ^ (32 :: Int)))

top :: Integer
top = (2 ^ (32 :: Int)) - 4   -- the largest aligned address

-- | A random initial state satisfying the contract, biased toward values
-- that matter for the given program: region sizes and register values
-- near the program's constants, pointers into and just around the data
-- region, regions at the bottom and at the top of the address space.
genInit :: Program -> G InitState
genInit prog = do
  let consts = programConstants prog
      nearSizes = [ s | k <- consts, d <- [negate 4, 0, 4], let s = alignDown (k + d), s >= 512, s <= 20000 ]
                  ++ [ 512 + s | k <- consts, d <- [negate 4, 0, 4], let s = alignDown (k + d), s >= 0, s <= 20000 ]
  size <- frequency
            [ (4, oneOf [512, 516, 520, 528, 544, 576, 640, 768, 1024, 1536, 2048])
            , (if null nearSizes then 0 else 2, oneOf nearSizes)
            , (3, fmap (\k -> 512 + (4 * k)) (range 0 1024))
            , (1, fmap (\k -> 512 + (4 * k)) (range 0 4096))
            ]
  zone <- frequency [(2, pure "low"), (2, pure "high"), (3, pure "mid")]
  db <- case zone of
          "low" -> fmap (* 4) (range 0 64)
          "high" -> do k <- range 0 64
                       pure ((top - (4 * k)) - size)
          _ -> fmap (* 4) (range 0 ((top - size) `div` 4))
  let dl = db + size
      pointers = [db, db + 4, db - 4, dl, dl - 4, dl + 4, dl - 8, dl - 512, db + alignDown (size `div` 2)]
  regs <- forM ([0] ++ [3 .. 9] ++ [11 .. 15]) (\k -> do
            v <- genValue consts pointers
            pure (k, v))
  fill <- frequency [ (3, fmap FillPattern word), (1, pure FillZero) ]
  -- plant program constants (and zeros) at the words the program is likely
  -- to read: offsets from DB and from DL
  let offs = accessOffsets prog
      sites = [ x | o <- offs, x <- [db + o, dl + o], (x `mod` 4) == 0, db <= x, x < dl ]
  k <- if null sites then pure (0 :: Int) else fmap fromInteger (range 0 3)
  plants <- replicateM k (do
              x <- oneOf sites
              v <- frequency [ (if null consts then 0 else 3, oneOf consts), (1, pure 0) ]
              pure (wordMod x, wordMod v))
  zeros <- frequency
             [ (3, pure [])
             , (1, fmap (\j -> [(wordMod (db + (4 * j)), 0)]) (range 0 32)) ]
  let st = InitState
             { iRegion = Region (wordMod db) (wordMod dl)
             , iRegs = M.fromList regs
             , iFill = fill
             , iWords = M.fromList (plants ++ zeros)
             }
  when (validInit st /= Right ()) (error ("genInit: invalid state " ++ show st))
  pure st

-- | Random initial state for the address-masking extension.
-- The region size is always a power of two.
genMaskingInit :: Program -> G InitState
genMaskingInit prog = do
  let consts = programConstants prog

  size <- oneOf [512, 1024, 2048, 4096, 8192, 16384]

  let maxBlock = (2 ^ (32 :: Int)) `div` size - 2
  block <- range 0 maxBlock
  let db = block * size

  let dl = db + size
      pointers =
        [ db
        , db + 4
        , db - 4
        , dl
        , dl - 4
        , dl + 4
        , dl - 8
        , dl - 512
        , db + alignDown (size `div` 2)
        ]

  regs <- forM ([0] ++ [3 .. 9] ++ [11 .. 15]) (\k -> do
            v <- genValue consts pointers
            pure (k, v))

  fill <- frequency
            [ (3, fmap FillPattern word)
            , (1, pure FillZero)
            ]

  let offs = accessOffsets prog
      sites =
        [ x
        | o <- offs
        , x <- [db + o, dl + o]
        , (x `mod` 4) == 0
        , db <= x
        , x < dl
        ]

  k <- if null sites
         then pure (0 :: Int)
         else fmap fromInteger (range 0 3)

  plants <- replicateM k (do
              x <- oneOf sites
              v <- frequency
                     [ (if null consts then 0 else 3, oneOf consts)
                     , (1, pure 0)
                     ]
              pure (wordMod x, wordMod v))

  zeros <- frequency
             [ (3, pure [])
             , (1, fmap
                    (\j -> [(wordMod (db + (4 * j)), 0)])
                    (range 0 32))
             ]

  let st = InitState
             { iRegion = Region (wordMod db) (wordMod dl)
             , iRegs = M.fromList regs
             , iFill = fill
             , iWords = M.fromList (plants ++ zeros)
             }

  when (validInit st /= Right ())
    (error ("genMaskingInit: invalid state " ++ show st))

  pure st


-- | A register value: small numbers, boundary values, the program's
-- constants and their neighbors, pointers into and just around the data
-- region (mostly aligned ones), and arbitrary words.
genValue :: [Integer] -> [Integer] -> G Word32
genValue consts pointers =
  fmap wordMod (frequency
    [ (3, frequency [ (3, fmap (* 4) (range (negate 4) 16)), (1, range (negate 16) 64) ])
    , (2, oneOf special)
    , (if null consts then 0 else 3, do k <- oneOf consts
                                        d <- frequency [ (4, oneOf [0, 0, negate 4, 4]), (1, oneOf [negate 1, 1]) ]
                                        pure (k + d))
    , (4, do p <- oneOf pointers
             d <- frequency [ (6, fmap (* 4) (range (negate 4) 4)), (1, range (negate 16) 16) ]
             pure (p + d))
    , (2, frequency [ (6, fmap (\w -> 4 * toInteger (w `div` 4)) word), (1, fmap toInteger word) ])
    ])
  where
    special = [ 0, 1, 2, 3, 4, 8, 511, 512, 4096, 2 ^ (16 :: Int), (2 ^ (31 :: Int)) - 4
              , 2 ^ (31 :: Int), 0xffff0000, negate 1, negate 4, negate 8, negate 12, negate 512 ]
