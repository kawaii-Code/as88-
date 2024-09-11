! BAD_VAR = THE_NUMBER_3 ! Doesn't work!
THE_NUMBER_3 = 2 * 1 - 4 / 4 + 2
HELLO_WORLD_LENGTH = hello_world_end - hello_world

.SECT .TEXT
	MOV AX, THE_NUMBER_3
	MOV BX, HELLO_WORLD_LENGTH
	! MOV CX, BAD_VAR ! This instruction will be silently removed because of the error

.SECT .DATA
hello_world:		.ASCII	"Hello, World!\n"
hello_world_end:	.WORD	0
