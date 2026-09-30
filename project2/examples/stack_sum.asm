;; stack_sum: stores 1..4 in the top four words of the data region and
;; adds them up (result 10).  Every access is r10-relative with a constant
;; offset in [-512, 0), so it is safe by construction and needs no check.
    mov r3, 1
    stxw [r10+-16], r3
    mov r3, 2
    stxw [r10+-12], r3
    mov r3, 3
    stxw [r10+-8], r3
    mov r3, 4
    stxw [r10+-4], r3
    mov r0, 0
    ldxw r1, [r10+-16]
    add r0, r1
    ldxw r1, [r10+-12]
    add r0, r1
    ldxw r1, [r10+-8]
    add r0, r1
    ldxw r1, [r10+-4]
    add r0, r1
    exit
