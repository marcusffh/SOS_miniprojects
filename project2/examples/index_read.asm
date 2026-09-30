;; index_read: returns word number r3 of the data region without comparing
;; the offset 4*r3 with the size r2 of the region: a violation whenever the
;; address r1 + 4*r3 (computed modulo 2^32) lies outside the region.
    mov r4, r3          ; 0
    lsh r4, 2           ; 1
    add r4, r1          ; 2
    ldxw r0, [r4+0]     ; 3
    exit                ; 4
