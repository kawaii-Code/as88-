! Computes '2x^4 - 3x^2 + x - 5' in x = 7

.SECT .TEXT
	MOV	AX, (x)
	ADD	CX, AX	! CX = x
	
	MUL	AX	! AX = x^2
	MOV	BX, AX
	MOV	DX, 3
	MUL	DX	! AX = 3 x^2
	SUB	CX, AX  ! CX = -3 x^2 + x
	MOV	AX, BX

	MUL	AX	! AX = x^4
	MOV	BX, AX
	MOV	DX, 2
	MUL	DX	! AX = 2 x^4
	ADD	CX, AX	! CX = 2 x^4 - 3 x^2 + x

	SUB	CX, 5	! CX = 2 x^4 - 3 x^2 + x - 5
	MOV	(res), CX

.SECT .DATA
x:	.WORD	7

.SECT .BSS
res:	.SPACE	2
