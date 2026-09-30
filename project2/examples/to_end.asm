;; to_end: a branch to address 3 of a program with three instructions, just
;; past its last instruction.  Runs with r3 = 1 take it and end in
;; violation.
    jeq r3, 1, +2       ; 0: to 3
    mov r0, 1           ; 1
    exit                ; 2
