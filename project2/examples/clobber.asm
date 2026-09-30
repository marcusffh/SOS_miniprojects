;; clobber: saves r1 and r2 in r6 and r7, then reuses r1 and r2 as
;; ordinary registers.  Reads the first and the last word of the data
;; region through the saved copies; never violates the policy.
    mov r6, r1          ; 0
    mov r7, r2          ; 1
    mov r1, 0           ; 2
    mov r2, 4096        ; 3
    ldxw r0, [r6+0]     ; 4: the first word
    mov r3, r6          ; 5
    add r3, r7          ; 6
    ldxw r4, [r3+-4]    ; 7: the last word
    add r0, r4          ; 8
    exit                ; 9
