! At end, AX = 15

.SECT .TEXT
	MOV CX, (iterations)
L1:
	ADD AX, 3
	LOOP L1

.SECT .DATA
iterations:	.WORD	5

.SECT .BSS
