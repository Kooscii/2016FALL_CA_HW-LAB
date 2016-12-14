	.file	"test.c"
	.def	___main;	.scl	2;	.type	32;	.endef
	.text
	.globl	_main
	.def	_main;	.scl	2;	.type	32;	.endef
_main:
	pushl	%ebp
	movl	%esp, %ebp
	andl	$-16, %esp
	subl	$2048, %esp
	call	___main
	movl	$0, 2044(%esp)
	movl	$0, 2044(%esp)
	jmp	L2
L5:
	movl	2044(%esp), %eax
	andl	$1, %eax
	testl	%eax, %eax
	jne	L3
	movl	2044(%esp), %eax
	movl	$2, (%esp,%eax,4)
L3:
	movl	2044(%esp), %eax
	andl	$3, %eax
	testl	%eax, %eax
	jne	L4
	movl	2044(%esp), %eax
	movl	$1, (%esp,%eax,4)
L4:
	addl	$1, 2044(%esp)
L2:
	cmpl	$510, 2044(%esp)
	jle	L5
	leave
	ret
	.ident	"GCC: (tdm-1) 4.9.2"
