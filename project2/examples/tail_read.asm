;; tail_read: the last word of the data region.  Never violates the policy,
;; but a rewriter without static analysis cannot know that.
    mov r3, r1          ; 0
    add r3, r2          ; 1: r3 = DL
    ldxw r0, [r3+-4]    ; 2
    exit                ; 3
