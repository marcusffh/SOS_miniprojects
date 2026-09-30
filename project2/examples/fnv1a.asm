;; fnv1a: 32-bit FNV-1a hash of the first 16 words of the data region,
;; taken as a sequence of 64 8-bit fields: the program loads each word and
;; takes it apart into four 8-bit fields, the least significant one first.
;; Immediates may be written as numbers up to 2^32-1.
    mov r0, 0x811c9dc5   ; 0: offset basis
    mov r3, 0            ; 1: offset i
    jge r3, 64, +13      ; 2: while i < 64 (to 16)
    mov r4, r1           ; 3
    add r4, r3           ; 4
    ldxw r5, [r4+0]      ; 5: the word at r1 + i
    mov r6, 4            ; 6: j = 4
    mov r7, r5           ; 7: next 8-bit field
    and r7, 0xff         ; 8
    xor r0, r7           ; 9
    mul r0, 0x01000193   ; 10: FNV prime
    rsh r5, 8            ; 11
    sub r6, 1            ; 12
    jne r6, 0, +-7       ; 13: to 7
    add r3, 4            ; 14
    ja -14               ; 15: to 2
    exit                 ; 16
