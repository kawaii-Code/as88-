! Test file with various garbage

.SECT .TEXT .HELLO
L1:   MOV ax, -32768 ! inline comment
    ADD ax, 3

.

.SECT .DATA

.SECT .BSS
