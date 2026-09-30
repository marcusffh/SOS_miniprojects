;; copy_reverse: reverses the order of the first 8 words of the data region
;; by way of the top 8 words of the region; returns 32.  Computed pointers
;; relative to r1 and to r10, and the memory is modified (the final memory
;; is part of the final configuration).
    mov r3, 32          ; 0: m = 32 (8 words span 32 addresses)
    mov r4, 0           ; 1: i
    jge r4, r3, +8      ; 2: to 11
    mov r5, r1          ; 3
    add r5, r4          ; 4
    ldxw r6, [r5+0]     ; 5: the word at r1+i
    mov r7, r10         ; 6
    sub r7, r4          ; 7
    stxw [r7+-4], r6    ; 8: to the word at r10-4-i
    add r4, 4           ; 9
    ja -9               ; 10: to 2
    mov r4, 0           ; 11
    jge r4, r3, +9      ; 12: to 22
    mov r7, r10         ; 13
    sub r7, r3          ; 14
    add r7, r4          ; 15
    ldxw r6, [r7+0]     ; 16: the word at r10-m+i
    mov r5, r1          ; 17
    add r5, r4          ; 18
    stxw [r5+0], r6     ; 19
    add r4, 4           ; 20
    ja -10              ; 21: to 12
    mov r0, r3          ; 22
    exit                ; 23
