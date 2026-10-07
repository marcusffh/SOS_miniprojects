# How to test our implementation

Our implementation contains two rewriters:

- `baseline` uses explicit address checks before memory accesses.
- `mask` uses address masking and requires the strengthened masking runtime assumptions.

Build the project first:

```bash
cabal build
```

## Baseline

Run 200 randomly generated programs using the baseline rewriter:

```bash
cabal run sfi-kit -- fuzz --programs 200 --rewriter "$(cabal list-bin sfi-rewrite) baseline"
```

Run the baseline rewriter on all supplied example programs:

```bash
for f in examples/*.asm; do
    out="/tmp/rewritten.asm"
    echo "===== Testing baseline: $f ====="
    cabal run sfi-rewrite -- baseline "$f" "$out" &&
    cabal run sfi-kit -- test "$f" "$out"
done
```

## Address masking

The address-masking extension requires the strengthened initial-state
assumptions described in the project specification. The `--masking` option
makes the test kit generate initial states satisfying these assumptions.

Run 200 randomly generated programs using address masking:

```bash
cabal run sfi-kit -- fuzz --masking --programs 200 --rewriter "$(cabal list-bin sfi-rewrite) mask"
```

Run the address-masking rewriter on all supplied example programs:

```bash
for f in examples/*.asm; do
    out="/tmp/rewritten.asm"
    echo "===== Testing masking: $f ====="
    cabal run sfi-rewrite -- mask "$f" "$out" &&
    cabal run sfi-kit -- test --masking "$f" "$out"
done
```

## Testing a single program

A single program can also be rewritten explicitly.

Baseline:

```bash
cabal run sfi-rewrite -- baseline examples/wild_pointer.asm out.asm
cabal run sfi-kit -- test examples/wild_pointer.asm out.asm
```

Address masking:

```bash
cabal run sfi-rewrite -- mask examples/wild_pointer.asm out.asm
cabal run sfi-kit -- test --masking examples/wild_pointer.asm out.asm
```


# Original supplied Readme

sfi-kit: baseline micro-eBPF for mini-project 2 (SOS 2026–27)
===============================================================

The kit implements **baseline micro-eBPF, and only baseline micro-eBPF**,
as defined in the reference definition (`mini-project-2-spec.pdf`):

* a parser and a printer for the concrete syntax (module `MicroEbpf.Syntax`);
* code addresses and a small assembler with symbolic labels
  (`MicroEbpf.Layout`);
* the well-formedness conditions W1–W3 for input programs (registers
  r0–r10) and output programs (r0–r15) (`MicroEbpf.WellFormed`);
* the data region and the initial states, a text format for them, and a
  random generator of initial states (`MicroEbpf.Contract`);
* the small-step semantics, parameterized by the data region, with the
  final configurations exit, error, trap and violation
  (`MicroEbpf.Semantics`);
* a tester for the requirements (0)–(2) on SFI rewriters
  (`MicroEbpf.Properties`) and a generator of random input programs for
  fuzzing (`MicroEbpf.GenProgram`);
* the command-line tool `sfi-kit`, a skeleton rewriter `sfi-rewrite` (the
  identity transformation), and example programs in `examples/`.

The kit implements none of the extensions of the project: there is no
`lddw`, no 64-bit value, no byte or halfword access, no other runtime
contract. If you work on an extension and want to use the kit, you have to
extend it yourselves (parser, code addresses and assembler, well-formedness
check, semantics, generators, tester). The kit is small (about 1,800 lines of Haskell,
comments included); read it when in doubt.

You are encouraged, but not required, to use the kit. You can build your
rewriter on the starter code of mini-project 1 instead (see "Using the
starter code of mini-project 1" below). In either case, your rewriter must
read and write the concrete syntax described here, and it will be tested
with `sfi-kit`.


Building
--------

Requires GHC (9.4–9.8) and cabal, as for the starter code of mini-project
1. The assembler library `ebpf-tools` is fetched from GitHub at the same
commit as in that starter code (see `cabal.project`).

    cabal build
    cabal run sfi-kit -- check examples/fnv1a.asm


The language
------------

Registers r0–r10 hold 32-bit words; r10 is the frame pointer and cannot
be written. Output programs may also use r11–r15. The instructions:

    add sub mul div mod or and xor lsh rsh arsh mov     d, s   or   d, k
    ldxw d, [s+o]        stxw [d+o], s
    ja o
    jeq jne jgt jge jlt jle jset jsgt jsge jslt jsle    d, s, o   or   d, k, o
    exit                 error

* An immediate k is an integer in [−2^31, 2^32), written in decimal or
  hexadecimal; it denotes a word: `and r1, -4` and `and r1, 0xfffffffc`
  are the same instruction.
* An offset o is an integer in [−2^15, 2^15). Negative offsets may be
  written `[r10-8]` or `[r10+-8]`, and `jeq r1, 0, -3` or `jeq r1, 0, +-3`;
  a non-negative jump offset may be written with or without `+`; `[s]`
  abbreviates `[s+0]`. The kit prints `[r10+-8]`, `jeq r1, 0, +-3`,
  `jeq r1, 0, +3` and `ja -3`.
* Arithmetic is 32-bit and is written without a suffix; the suffix `32`
  (`add32 r1, r2`) is accepted and means the same. The suffix `64` is
  rejected: micro-eBPF has no 64-bit instructions.
* `error` ends the run in the final configuration error; there is no
  `call` instruction.
* One instruction per line; comments start with `;`.

Code addresses count instructions from 0: a jump at code address a with
offset o continues at a + 1 + o, and the kit reports code addresses as
`pc`. A jump target may lie outside the code region [0, #P); a run that
takes such a jump ends in violation. Well-formedness (`sfi-kit check`):
W1 (at least one instruction; only the instructions above; registers
r0–r10, or r0–r15 with `--output`; immediates and offsets in range), W2 (no
instruction writes r10), W3 (the last instruction is `ja`, `exit` or
`error`). The parser rejects every other instruction (W1), and numbers
whose absolute value is 2^63 or more.


The semantics
-------------

A run starts in an initial state: a data region [DB, DL) (DB and DL
multiples of 4, DB < DL < 2^32, DL − DB ≥ 512), r1 = DB, r2 = DL − DB,
r10 = DL, arbitrary values in all other registers (r11–r15 included), and
an arbitrary word at every aligned address of the data region. Every step
executes one instruction:

* an instruction whose next code address lies outside the code region, or
  a load or store whose address (computed modulo 2^32) lies outside the
  data region, ends the run in **violation**, before it has any effect;
* a load or store whose address lies in the data region but is not a
  multiple of 4 ends the run in **trap**;
* `exit` and `error` end the run in **exit** and **error**;
* every other step updates the program pointer, the registers and the
  memory as defined in section 3.2 of the reference definition.

`sfi-kit run` shows the final configuration: its kind, r0–r10 (r0–r15 if
the program uses r11–r15), and the memory words that the run changed.


Commands
--------

    sfi-kit check [--output] FILE          well-formedness of an input program
                                           (r0-r10), or of an output program
                                           (r0-r15) with --output
    sfi-kit print FILE                     parse and print
    sfi-kit run [--trace] [--init FILE | --seed S] FILE
                                           run once and show the final
                                           configuration
    sfi-kit test [OPTIONS] ORIGINAL REWRITTEN
                                           test requirements (0)-(2) of one
                                           rewriting on many initial states
    sfi-kit fuzz [OPTIONS] --rewriter CMD  random input programs P; calls
                                           `CMD IN OUT' and tests P against
                                           the output
    sfi-kit gen [--seed S] [-n N] DIR      write N (default 100) random input
                                           programs to DIR

Options of `test` and `fuzz`:

    -n N            random initial states per program (test: 1000, fuzz: 200)
    --seed S        seed of the generators (default 1)
    --inputs FILE   additional hand-written initial states (format below)
    --fuel F        step budget of the original program (default 100000)
    --overhead K    the rewritten program gets K*F steps (default 50)
    --programs N    fuzz: number of programs (default 200)

The verdict for one initial state (section 5 of the reference definition):
*fail (2)* if the rewritten program ends in violation; *inconclusive* if
the original does not end within F steps; *pass* if the original ends in
violation (inconclusive if the rewritten program does not end within K*F
steps); *fail (1)* if the original ends in exit, error or trap and the
rewritten program does not end within K*F steps, or ends in a final
configuration that does not agree (other kind, other r0–r10, or other
memory); *pass* otherwise. `sfi-kit fuzz` in addition counts a failure of
the rewriter, or an output that is not a well-formed output program, as a
failure of (0).

A typical session with the skeleton rewriter:

    cabal run sfi-rewrite -- examples/wild_pointer.asm out.asm
    cabal run sfi-kit -- test examples/wild_pointer.asm out.asm
    cabal run sfi-kit -- fuzz --programs 500 --rewriter "$(cabal list-bin sfi-rewrite)"

When a test fails, `sfi-kit` prints the initial state in the format below.
Save it to a file and replay it with `sfi-kit run --init FILE --trace` on
the original and on the rewritten program; the last lines say how the run
ended and why. `sfi-kit fuzz` keeps its programs in `fuzz-out/`.


Initial states
--------------

    # comments start with #
    base 0x20000            # DB (r1); r2 = DL - DB
    limit 0x20800           # DL (r10)
    fill zero               # or: fill pattern N (pseudo-random words)
    mem 0x20000 0x11 0x22   # the words at 0x20000, 0x20004, ...
    r3 0x2a                 # r0, r3-r9, r11-r15 (default 0)
    r5 -4                   # = 0xfffffffc
    ---                     # separates initial states

`sfi-kit` checks every hand-written state against the contract (DB and DL
multiples of 4, DB < DL, DL − DB ≥ 512, memory words at aligned addresses
of the data region). The random generator places the data region at the
bottom, at the top or in the middle of the address space, prefers sizes
and register values near the constants of the program under test and
boundary values such as 0xfffffffc, generates addresses in and just
around the data region (mostly aligned ones), and places constants of the
program at addresses that the program is likely to read. `examples/inputs.txt` contains
three hand-written states.


Using the library in your rewriter
----------------------------------

    MicroEbpf.Syntax      parseProgramFile, showProgram, showInstruction,
                          the pattern Error
    MicroEbpf.Layout      addressed, jumpTarget,
                          Item (Label, Ins, JmpTo, JCondTo), labeled, assemble
    MicroEbpf.WellFormed  wellFormed Input / wellFormed Output,
                          writtenRegister, registersOf, jumpsOutside
    MicroEbpf.Semantics   run, runTrace, step (for your own tests)
    MicroEbpf.Contract    InitState, genInit, parseInits, renderInit
    MicroEbpf.Properties  checkState, testRewriting

Programs are lists of the instruction type `Instruction` of `ebpf-tools`
(module `Ebpf.Asm`): an arithmetic instruction is `Binary B32 op d x`, and
`error` is `Error`, a pattern synonym exported by `MicroEbpf.Syntax` (use it
in expressions and patterns; with an explicit import list, write
`import MicroEbpf.Syntax (pattern Error)` and enable the extension
`PatternSynonyms`). `labeled` turns a program into items: a label (its
code address) in front of every instruction, and symbolic targets for the
jumps whose target lies in the code region. Jumps whose target lies
outside the code region stay numeric `Ins` items, and your rewriter has to
decide what becomes of them. `assemble` turns items back into a program
and computes every offset. Where you put the label of an instruction
relative to the code you insert in front of it is a design decision that
matters.


Using the starter code of mini-project 1
----------------------------------------

The assembler of the starter code (`ebpf-tools`) reads and writes a syntax
close to that of micro-eBPF. A rewriter built on it has to handle the
differences:

* it does not know `error`: replace `error` by `call 0` before parsing (as
  the kit does), and print the instruction `Call 0` as `error`, since the
  kit rejects `call`;
* it reads an unsuffixed arithmetic instruction (`add r1, r2`) as a 64-bit
  instruction (`Binary B64 ...`) and prints that as `add64 r1, r2`, which
  the kit rejects; print it without a suffix, or as `add32`. A static
  analysis must use the 32-bit semantics of micro-eBPF;
* it rejects the forms `[r10-8]`, `jeq r1, 0, -3` and `jeq r1, 0, 3` and
  requires `[r10+-8]`, `jeq r1, 0, +-3` and `jeq r1, 0, +3`. The examples
  and the programs that `sfi-kit` generates use only the latter forms, but
  hand-written test programs may use the former; normalize the input as
  `MicroEbpf.Syntax` does (functions `normalizeMemRefs` and
  `normalizeJump`).

Its printer writes negative offsets as `[r10 +-8]`, `jeq r1, 0, +-3` and
`ja +-3`; the kit accepts all three.


Examples
--------

Every example starts with a comment that says what it does and what makes
it interesting for a rewriter.

    branch_back    a branch to address -1, before the start of the program
    branch_out     a branch far beyond the end of the program
    to_end         a branch to address #P, just past the last instruction
    dead_jump      a jump outside the code region that no run executes
    checksum       16-bit one's-complement sum over the data region
    clobber        saves r1 and r2, then reuses them
    copy_reverse   reverses 8 words by way of the stack; changes memory
    error_check    compares an index with the size and ends with error
    fnv1a          FNV-1a hash of the first 16 words of the data region
    index_read     reads word number r3 without comparing it with the size
    misaligned     a load at r1 + 2: always ends in trap
    nested_loops   an 8x8 matrix on the stack
    offset_wrap    an access near the top of the address space
    packet_parse   classifies an Ethernet frame at the start of the region
    past_end       sometimes stores to the word at r10, just past the region
    scan_zero      scans for a zero word without a size check
    stack_array    computed indices into an array on the stack
    stack_sum      only accesses that are safe by construction
    sum_five       no memory accesses at all
    sum_words      a loop over the whole data region, bounded by r2
    tail_read      the last word of the data region
    uninit_read    reads a word that the program never wrote
    wild_pointer   dereferences an arbitrary register
    wraparound     pointer arithmetic that wraps around 2^32
    inputs.txt     three hand-written initial states (--inputs)
