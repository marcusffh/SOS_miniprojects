;; wraparound: pointer arithmetic that wraps around 2^32 lands back at r1,
;; so the load is permitted in every run.
    mov r3, 0x80000000   ; 0: 2^31
    mov r4, r1           ; 1
    add r4, r3           ; 2
    add r4, r3           ; 3: r4 = r1 + 2^32 = r1 (modulo 2^32)
    ldxw r0, [r4+0]      ; 4
    exit                 ; 5
