;; offset_wrap: an access near the top of the address space.  With
;; r6 = 0xfffffff4 the address is 0xfffffffc, the last word of the address
;; space: a check that compares x + 4 with DL computes x + 4 = 0 and lets
;; the access pass.  With r6 = 0xfffffff8 or 0xfffffffc the address itself
;; wraps around, to 0 or 4.
    ldxw r0, [r6+8]
    exit
