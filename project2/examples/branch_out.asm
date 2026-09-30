;; branch_out: a conditional branch to address 41 of a program with three
;; instructions.  Runs with an odd r3 take it and end in violation.
    jset r3, 1, +40     ; 0: to 41
    mov r0, 1           ; 1
    exit                ; 2
