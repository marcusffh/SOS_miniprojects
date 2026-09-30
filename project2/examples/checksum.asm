;; checksum: 16-bit one's-complement sum over the data region: the two
;; 16-bit halves of every word are added up, then the carries are folded
;; back in.
    mov r0, 0           ; 0: sum
    mov r3, 0           ; 1: offset i
    jge r3, r2, +10     ; 2: while i < r2 (to 13)
    mov r5, r1          ; 3
    add r5, r3          ; 4
    ldxw r6, [r5+0]     ; 5
    mov r7, r6          ; 6
    and r7, 0xffff      ; 7: low half
    add r0, r7          ; 8
    rsh r6, 16          ; 9: high half
    add r0, r6          ; 10
    add r3, 4           ; 11
    ja -11              ; 12: to 2
    mov r4, r0          ; 13: fold the carries
    rsh r4, 16          ; 14
    jeq r4, 0, +3       ; 15: to 19
    and r0, 0xffff      ; 16
    add r0, r4          ; 17
    ja -6               ; 18: to 13
    xor r0, 0xffff      ; 19
    exit                ; 20
