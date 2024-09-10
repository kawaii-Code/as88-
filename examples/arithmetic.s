.SECT .TEXT
	MOV AX, 2 + 3 * 4	! AX = 14
	MOV BX, -5 / 3		! BX = -2 (rounded towards -inf)
	MOV CX, y - x		! CX = 4  (len("1234") == 4)
	MOV DX, 2 * z		! DX = 12 (sizeof(x) == 4, sizeof(y) == 2, z = 6)

.SECT .DATA
x:  .ASCII "1234"	! No null terminator. Use .ASCIZ for it
y:  .WORD   16
z:  .WORD   0
