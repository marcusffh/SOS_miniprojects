;; uninit_read: reads a word that the program never wrote.  Permitted: the
;; value is whatever the initial memory holds.
    ldxw r0, [r10+-16]
    exit
