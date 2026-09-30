;; past_end: when the input value r3 is odd, writes the word at r10, just
;; past the end of the data region: a violation.
    mov r0, 0           ; 0
    mov r4, 7           ; 1
    jset r3, 1, +2      ; 2: to 5
    stxw [r10+-4], r4   ; 3
    ja +1               ; 4: to 6
    stxw [r10+0], r4    ; 5: the word at DL
    exit                ; 6
