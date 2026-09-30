;; scan_zero: the number of words up to and including the first zero word
;; at the start of the data region.  There is no comparison with the size
;; of the region, so the run ends in violation when the region contains no
;; zero word.  The back edge goes to the load itself: a rewriter must
;; direct it to the check in front of the load.
    mov r0, 0           ; 0
    mov r3, r1          ; 1
    ldxw r4, [r3+0]     ; 2: loop
    add r3, 4           ; 3
    add r0, 1           ; 4
    jne r4, 0, +-4      ; 5: to 2
    exit                ; 6
