;; stack_array: a[i] = i*i for an array a of 16 words at [r10-64, r10),
;; then returns a[r3 % 16] for the arbitrary input value r3.  The pointers
;; are computed, so a rewriter without static analysis must check both
;; accesses; an interval analysis proves them safe.  (The program uses r1
;; as a loop counter: that is legal.)
    mov r1, 0           ; 0: i = 0
    mov r4, r1          ; 1: loop: p = r10 - 64 + 4*i
    lsh r4, 2           ; 2
    add r4, r10         ; 3
    add r4, -64         ; 4
    mov r5, r1          ; 5
    mul r5, r1          ; 6
    stxw [r4+0], r5     ; 7: *p = i*i
    add r1, 1           ; 8
    jlt r1, 16, +-9     ; 9: to 1
    mod r3, 16          ; 10: unsigned, so 0 <= r3 < 16
    lsh r3, 2           ; 11
    add r3, r10         ; 12
    ldxw r0, [r3+-64]   ; 13
    exit                ; 14
