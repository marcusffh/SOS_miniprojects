;; nested_loops: m[i][j] = i + j in an 8x8 matrix of words at
;; [r10-256, r10), then returns the sum of the diagonal (56).
    mov r3, 0           ; 0: i
    mov r4, 0           ; 1: outer loop: j = 0
    mov r5, r3          ; 2: inner loop: p = r10 - 256 + 4*(8*i + j)
    lsh r5, 3           ; 3
    add r5, r4          ; 4
    lsh r5, 2           ; 5
    add r5, r10         ; 6
    mov r6, r3          ; 7
    add r6, r4          ; 8
    stxw [r5+-256], r6  ; 9
    add r4, 1           ; 10
    jlt r4, 8, +-10     ; 11: to 2
    add r3, 1           ; 12
    jlt r3, 8, +-13     ; 13: to 1
    mov r0, 0           ; 14
    mov r3, 0           ; 15
    mov r5, r3          ; 16: diagonal: p = r10 - 256 + 36*i
    mul r5, 36          ; 17
    add r5, r10         ; 18
    ldxw r6, [r5+-256]  ; 19
    add r0, r6          ; 20
    add r3, 1           ; 21
    jlt r3, 8, +-7      ; 22: to 16
    exit                ; 23
