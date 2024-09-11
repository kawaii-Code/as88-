.SECT .TEXT
	MOV AX, (hello_world)	! AX contains "He"
	MOV BX, hello_world_end - hello_world	! BX = 15 (length of "Hello, World\n" + 0 terminator

.SECT .DATA
hello_world:	.ASCIZ	"Hello, World!\n" ! 0-terminated
hello_world_end:	.BYTE	0

.SECT .BSS
