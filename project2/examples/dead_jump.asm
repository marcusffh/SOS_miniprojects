;; dead_jump: a jump to address 10, outside the code region, after the
;; final exit.  No run executes it, so every run ends in exit; the
;; rewritten program must behave the same.
    mov r0, 1           ; 0
    exit                ; 1
    ja +7               ; 2: to 10, never executed
