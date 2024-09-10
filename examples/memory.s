.SECT .TEXT
	MOV AX, (x)	! AX = 2
	ADD AX, (y)	! AX = 5
	ADD (x), AX	! x = 7
	ADD (x), AX	! x = 12

.SECT .DATA
x:	.WORD	2
y:	.WORD	3

.SECT .BSS

