;; error_check: like index_read, but compares the offset 4*r3 (computed
;; modulo 2^32) with the size r2 first and ends with error if it lies
;; outside the region.  Never violates the policy; the rewritten program
;; must end with error in exactly the same runs.
    mov r4, r3          ; 0
    lsh r4, 2           ; 1: offset 4*r3
    jge r4, r2, +3      ; 2: outside the region (to 6)
    add r4, r1          ; 3
    ldxw r0, [r4+0]     ; 4
    exit                ; 5
    error               ; 6
