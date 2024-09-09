.SECT .TEXT
	MOV AX, (hello_world)

.SECT .DATA
hello_world:	.ASCIZ	"Hello, World!\n" ! 0-terminated

.SECT .BSS
