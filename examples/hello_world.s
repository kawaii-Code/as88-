! Simple "hello world"

	_EXIT	= 1
	_WRITE	= 4
	_STDOUT	= 1

.SECT .TEXT
start:
	MOV	CX, hello_world_end - hello_world
	PUSH	CX
	PUSH	hello_world
	PUSH	_STDOUT
	PUSH	_WRITE
	SYS

	ADD	SP, 8
	SUB	CX, AX
	PUSH	CX
	PUSH	_EXIT
	SYS

.SECT .DATA
hello_world:	.ASCII "Hello World\n"
hello_world_end:	.BYTE 0

.SECT .BSS
