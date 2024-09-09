! 1: AX = 0
! 2: AX = 0
! 3: AX = 3

.SECT .TEXT
	MOV AX, (x)
	ADD (x), 3
	ADD AX, (x)

.SECT .DATA

.SECT .BSS
x:	.SPACE	8
