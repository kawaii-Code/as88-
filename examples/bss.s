.SECT .TEXT
	MOV AX, (x)	! AX = 0
	ADD (x), 3	! x = 3
	ADD AX, (x)	! AX = 3

.SECT .DATA

.SECT .BSS
x:	.SPACE	8
