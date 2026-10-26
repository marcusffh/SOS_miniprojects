# SOS_miniprojects

Information-flow analysis: determine whether sensitive information can influence values that should remain public. 

How can information leak?
We need to know not just what value a register contains but also where its value came from
We use two security levels: 
Low - public information
High - sensitive or secret information

We want to track whether sensitive information can influence other values as the program executes

Problem A: direct information flow
Suppose we have
r1 = secret (High)
r0 = r1 (copy)

If we only track values, we know that r0 receives the value of r1, but we also need to know that r0 now contains sensitve information.

We introduces security states that track labels for registers and memory. Instructions such as Mov propagate the source's security level to the destination. Arirthmetic instructions combine the security levels of their operand so if either operand of an addition is High the result is High

Problem B: information flow through memory 
If we have:
Store r1 at memory adress 10
Load memory address 10 into r0

if r1 is High, the value written to memory is sensitive. If our analysis tracks only registers, it loses that information when the value is stored.

We extended the security state to track both registers and memory.
That means a store transfers the relevant security level to the memory cell, and a later load transfers the memory cell's level to the destination register.
We also handled uncertain addresses and uninitialized memory conservatively. If the analysis cannot establish that a memory read is safe to treat as Low, defaults to High rather than incorrectly declaring the value public.

Problem C: indirect information flow through branches
consider:
if (secret == 0) {
    r0 = 0
} else {
    r0 = 1
}
Neither assignment uses the secret directly. Both assign public constants.
But if the final value of r0 reveals something about the secret: it is 0 or 1 depending on the secret condition
If we only track security levels of instruction operands, we would miss this leak

We fixed it by accounting for control dependencies
1. Determine whether a branch condition depends on High information
2. Identify which instructions are controlled by that branch
3. Treat assignments inside that regions as influenced by the branch condition
4. Stop that influence when exevutuion reaches the branch's reconvergence point
