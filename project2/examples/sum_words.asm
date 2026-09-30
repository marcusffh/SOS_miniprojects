;; sum_words: sum (modulo 2^32) of the words of the data region [r1, r10),
;; whose size is r2.  The loop compares the offset with r2, so the program
;; never violates the policy.
    mov r0, 0           ; 0
    mov r3, 0           ; 1: offset i = 0
    jge r3, r2, +6      ; 2: while i < r2 (to 9)
    mov r4, r1          ; 3
    add r4, r3          ; 4
    ldxw r5, [r4+0]     ; 5
    add r0, r5          ; 6
    add r3, 4           ; 7
    ja -7               ; 8: to 2
    exit                ; 9
