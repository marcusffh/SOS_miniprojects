;; branch_back: a branch to address -1, before the start of the program,
;; taken when r4 > 100 (unsigned): such runs end in violation.
    jgt r4, 100, +-2    ; 0: to -1
    mov r0, 1           ; 1
    exit                ; 2
