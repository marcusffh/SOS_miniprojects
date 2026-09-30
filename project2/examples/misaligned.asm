;; misaligned: a load at r1 + 2, an address in the data region that is not
;; a multiple of 4: the run ends in trap, whatever the initial state.  A
;; rewriter need not check alignment, but it must not change the address
;; of an access either.
    ldxw r0, [r1+2]
    exit
