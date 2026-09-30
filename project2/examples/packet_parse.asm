;; packet_parse: classifies an Ethernet frame at the start of the data
;; region.  The frame is stored as a sequence of 8-bit fields (octets),
;; four per word, the first one in the least significant bits; octet k of
;; the frame lies in the word at r1 + 4*(k/4), in bits 8*(k%4) to
;; 8*(k%4)+7.  The frame length n (in octets) is taken to be r2 - 512 (the
;; top 128 words of the region serve as stack).  Returns 0 (too short),
;; 1 (not IPv4), 2 (IPv4, other), 6 (TCP), 17 (UDP).  The program compares
;; n with the offsets it reads, so it never violates the policy.
    mov r7, r2          ; 0
    sub r7, 512         ; 1: n
    mov r0, 0           ; 2
    jlt r7, 16, +21     ; 3: too short (to 25)
    ldxw r3, [r1+12]    ; 4: octets 12-15
    mov r4, r3          ; 5
    mov r5, r3          ; 6
    and r5, 0xff        ; 7: octet 12
    lsh r5, 8           ; 8
    rsh r3, 8           ; 9
    and r3, 0xff        ; 10: octet 13
    or r3, r5           ; 11: EtherType, octets 12 and 13 in network order
    mov r0, 1           ; 12
    jne r3, 0x0800, +11 ; 13: not IPv4 (to 25)
    mov r0, 0           ; 14
    jlt r7, 34, +9      ; 15: no room for an IPv4 header (to 25)
    rsh r4, 20          ; 16: IP version: upper 4 bits of octet 14
    and r4, 0xf         ; 17
    mov r0, 1           ; 18
    jne r4, 4, +5       ; 19: (to 25)
    ldxw r0, [r1+20]    ; 20: octets 20-23
    rsh r0, 24          ; 21: protocol (octet 23)
    jeq r0, 6, +2       ; 22: (to 25)
    jeq r0, 17, +1      ; 23: (to 25)
    mov r0, 2           ; 24
    exit                ; 25
