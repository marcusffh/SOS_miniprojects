{-# LANGUAGE PatternSynonyms #-}
-- | Random well-formed input programs, for fuzzing a rewriter.
--
-- The programs terminate (forward branches, counted loops and scans with
-- a constant cap).  They access the top of the data region through r10
-- and through derived pointers, and the bottom through r1 or a copy of it;
-- they compute pointers, use pointers taken from arbitrary registers,
-- sometimes compare pointers with the bounds of the region themselves,
-- sometimes get these comparisons wrong, sometimes overwrite r1 and r2,
-- sometimes end with the error instruction, and sometimes make misaligned
-- accesses.  About a quarter of the programs contain a jump or branch to
-- an address outside the code region (taken in some runs, or in dead
-- code).
module MicroEbpf.GenProgram
  ( genProgram
  ) where

import Ebpf.Asm
import MicroEbpf.GenUtil
import MicroEbpf.Layout
import MicroEbpf.Syntax (pattern Error)

data Env = Env
  { eBase :: Reg        -- ^ register holding DB
  , eSize :: Reg        -- ^ register holding DL - DB
  , eProtected :: [Reg] -- ^ registers that must not be written here
  , eDepth :: Int
  }

-- | Generate a program.
genProgram :: G Program
genProgram = do
  save <- chance 7 10
  let env0 = if save
               then Env (Reg 6) (Reg 7) [Reg 6, Reg 7] 0
               else Env (Reg 1) (Reg 2) [Reg 1, Reg 2] 0
      prologue = if save then [Ins (mov (Reg 6) (R (Reg 1))), Ins (mov (Reg 7) (R (Reg 2)))] else []
  k <- range 3 12
  (body, _) <- stmts env0 k 0
  r0 <- oneOf [[], [Ins (mov (Reg 0) (Imm 0))]]
  let items = prologue ++ (r0 ++ (body ++ [Ins Exit]))
  p <- case assemble items of
         Right p -> pure p
         Left err -> error ("genProgram: " ++ err)
  outside <- chance 1 4
  if outside then jumpOutside p else pure p

-- | Add a jump or branch to an address outside the code region.  Adding
-- instructions at the start or at the end leaves the (relative) jumps of
-- the program intact.
jumpOutside :: Program -> G Program
jumpOutside p = do
  let n = toInteger (length p)
  kind <- range 1 5
  far <- range 0 40
  case kind of
    1 -> -- a branch beyond the end, taken by the runs with an odd r3
      pure (JCond Jset (Reg 3) (Imm 1) (fromInteger (n + far)) : p)
    2 -> -- a branch before the start, taken by the runs with r4 > 100
      pure (JCond Jgt (Reg 4) (Imm 100) (fromInteger (negate (2 + far))) : p)
    3 -> -- a jump out of the code region in dead code after the final exit
      pure (p ++ [Jmp (fromInteger (1 + far))])
    4 -> -- a jump out of the code region that is always skipped
      pure ([Jmp 1, Jmp (fromInteger (n + far))] ++ p)
    _ -> -- a branch to the address just past the last instruction, taken
         -- by the runs in which bit 1 of r3 is set
      pure (JCond Jset (Reg 3) (Imm 2) (fromInteger n) : p)

mov :: Reg -> RegImm -> Instruction
mov r ri = Binary B32 Mov r ri

alu :: BinAlu -> Reg -> RegImm -> Instruction
alu op r ri = Binary B32 op r ri

immI :: Integer -> RegImm
immI k = Imm (fromInteger k)

ldxw :: Reg -> Reg -> Integer -> Instruction
ldxw d s o = Load B32 d s (Just (fromInteger o))

stxw :: Reg -> Integer -> Reg -> Instruction
stxw d o s = Store B32 d (Just (fromInteger o)) (R s)

stmts :: Env -> Integer -> Int -> G ([Item Int], Int)
stmts _ 0 lbl = pure ([], lbl)
stmts env k lbl = do
  (s, lbl1) <- stmt env lbl
  (rest, lbl2) <- stmts env (k - 1) lbl1
  pure (s ++ rest, lbl2)

scratch :: Env -> G Reg
scratch env = scratchExcept env []

scratchExcept :: Env -> [Reg] -> G Reg
scratchExcept env avoid =
  oneOf [ r | r <- map Reg [0, 3, 4, 5, 8, 9], r `notElem` (eProtected env ++ avoid) ]

anyReg :: G Reg
anyReg = oneOf (map Reg [0 .. 10])

-- | A byte offset or displacement in [lo, hi]: mostly a multiple of 4
-- (otherwise an access with it traps).
offsetIn :: Integer -> Integer -> G Integer
offsetIn lo hi =
  frequency
    [ (30, fmap (* 4) (range (ceilDiv lo 4) (hi `div` 4)))
    , (1, range lo hi) ]
  where
    ceilDiv a b = negate ((negate a) `div` b)

-- | An offset from r10: mostly an aligned one in [-512, 0), sometimes one
-- at or just beyond the ends of that range, rarely a misaligned one.
stackOffset :: G Integer
stackOffset =
  frequency
    [ (30, do k <- range 1 128
              pure (negate (4 * k)))
    , (3, oneOf [negate 520, negate 516, 0, 4, 8])
    , (1, oneOf [negate 513, negate 2, negate 1])
    ]

stmt :: Env -> Int -> G ([Item Int], Int)
stmt env lbl = do
  let deep = eDepth env >= 2
  choice <- frequency
              ( [ (3, pure k) | k <- [1 .. 9 :: Int] ]
                ++ [ (1, pure 10), (1, pure 11) ]   -- wild pointers and error: rarer
                ++ (if deep then [] else [ (3, pure 12), (3, pure 13), (3, pure 14) ]) )
  case choice of
    1 -> do  -- arithmetic
      d <- scratch env
      op <- oneOf [Add, Sub, Mul, Div, Or, And, Lsh, Rsh, Mod, Xor, Mov, Arsh]
      src <- frequency [(1, fmap R anyReg), (1, fmap immI (range (negate 20) 300))]
      pure ([Ins (alu op d src)], lbl)
    2 -> do  -- a large constant
      d <- scratch env
      v <- oneOf [0x80000000, 0xfffffffc, negate 1, 0x12345678, 7]
      pure ([Ins (mov d (Imm (fromInteger v)))], lbl)
    3 -> do  -- a store near the top of the region, mostly below r10
      off <- stackOffset
      src <- anyReg
      pure ([Ins (stxw (Reg 10) off src)], lbl)
    4 -> do  -- a load near the top of the region
      off <- stackOffset
      d <- scratch env
      pure ([Ins (ldxw d (Reg 10) off)], lbl)
    5 -> do  -- an access near the bottom of the region, through the base
      off <- offsetIn (negate 4) 40
      d <- scratch env
      store <- chance 1 3
      pure ( [ if store then Ins (stxw (eBase env) off d) else Ins (ldxw d (eBase env) off) ]
           , lbl )
    6 -> do  -- a computed pointer
      p <- scratch env
      d <- scratch env
      idx <- frequency [(5, fmap immI (offsetIn 0 64)), (1, fmap R anyReg)]
      pure ( [ Ins (mov p (R (eBase env)))
             , Ins (alu Add p idx)
             , Ins (ldxw d p 0) ]
           , lbl )
    7 -> do  -- a pointer below r10 in another register
      p <- scratch env
      t <- scratchExcept env [p]
      k <- offsetIn 0 520
      off <- offsetIn (negate 8) 8
      v <- range 0 255
      pure ( [ Ins (mov p (R (Reg 10)))
             , Ins (alu Sub p (Imm (fromInteger k)))
             , Ins (mov t (Imm (fromInteger v)))
             , Ins (stxw p off t) ]
           , lbl )
    8 -> do  -- an access that the program checks itself; sometimes the
             -- check is off by one word
      e <- scratch env
      k <- offsetIn 0 64
      d <- scratchExcept env [e]
      sloppy <- chance 1 4
      let end = if sloppy then k else k + 4
          skip = lbl
      pure ( [ Ins (mov e (R (eBase env)))
             , Ins (alu Add e (Imm (fromInteger end)))
             , Ins (mov d (R (eBase env)))
             , Ins (alu Add d (R (eSize env)))
             , JCondTo Jgt e (R d) skip
             , Ins (ldxw d (eBase env) k)
             , Label skip ]
           , lbl + 1 )
    9 -> do  -- overwrite r1 or r2
      r <- oneOf [Reg 1, Reg 2]
      v <- range 0 1000
      if r `elem` eProtected env
        then pure ([], lbl)
        else pure ([Ins (mov r (Imm (fromInteger v)))], lbl)
    10 -> do  -- an access through an arbitrary register: a wild pointer
      r <- anyReg
      off <- offsetIn (negate 8) 8
      d <- scratch env
      store <- chance 1 3
      pure ( [ if store then Ins (stxw r off d) else Ins (ldxw d r off) ]
           , lbl )
    11 -> do  -- end with error under a condition
      x <- anyReg
      cmp <- oneOf [Jle, Jlt, Jne, Jsge]
      y <- frequency [(2, fmap immI (range 0 200)), (1, fmap R anyReg)]
      let skip = lbl
      pure ([JCondTo cmp x y skip, Ins Error, Label skip], lbl + 1)
    12 -> do  -- a forward conditional skip over a block
      x <- anyReg
      cmp <- oneOf [Jeq, Jne, Jgt, Jge, Jlt, Jle, Jset, Jsgt, Jsge, Jslt, Jsle]
      y <- frequency [(2, fmap immI (range (negate 2) 64)), (1, fmap R anyReg)]
      k <- range 1 3
      let skip = lbl
      (inner, lbl') <- stmts (env { eDepth = eDepth env + 1 }) k (lbl + 1)
      pure ([JCondTo cmp x y skip] ++ (inner ++ [Label skip]), lbl')
    13 -> do  -- a counted loop
      c <- oneOf [ r | r <- [Reg 8, Reg 9], r `notElem` eProtected env ]
      n <- range 0 12
      k <- range 1 3
      let hd = lbl
          env' = env { eDepth = eDepth env + 1, eProtected = c : eProtected env }
      (inner, lbl') <- stmts env' k (lbl + 1)
      pure ( [Ins (mov c (Imm (fromInteger n))), Label hd]
               ++ (inner ++ [ Ins (alu Sub c (Imm 1)), JCondTo Jsgt c (Imm 0) hd ])
           , lbl' )
    _ -> do  -- a scan of the region from its base (bound: its size, capped);
             -- the sloppy variant reads the word at DL
      sloppy <- chance 1 4
      cap <- frequency [(1, range 1 100), (1, pure 100000)]
      let hd = lbl
          out = lbl + 1
          i = Reg 3
          p = Reg 4
          x = Reg 5
          leave = if sloppy then Jgt else Jge
      if any (`elem` eProtected env) [i, p, x]
        then pure ([], lbl)
        else pure ( [ Ins (mov i (Imm 0))
                    , Ins (mov (Reg 0) (Imm 0))
                    , Label hd
                    , JCondTo Jge i (Imm (fromInteger (4 * cap))) out
                    , JCondTo leave i (R (eSize env)) out
                    , Ins (mov p (R (eBase env)))
                    , Ins (alu Add p (R i))
                    , Ins (ldxw x p 0)
                    , Ins (alu Add (Reg 0) (R x))
                    , Ins (alu Add i (Imm 4))
                    , JmpTo hd
                    , Label out ]
                  , lbl + 2 )
