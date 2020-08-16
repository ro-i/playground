.intel_syntax noprefix
.global brainfuck


// library symbols
.extern ferror
.extern free
.extern getchar
.extern memset
.extern putchar
.extern stdin


// cf. global variables in brainfuck.c


#define GET_RDI ([rbp - 6 * 8])
#define GET_EOF_BEHAVIOR ([rbp - 7 * 8])
#define GET_MEMORY_BUF_LIMIT ([rbp - 8 * 8])
#define GET_ASM_BUFSIZ ([rbp - 9 * 8])


.section .text

.macro easy_call func
	// save
	push rsi

	// check alignment - (16 or 8) byte are the only possible cases here
	test rsp, 0xf
	jnz 0f

	sub rsp, 0x8
	call \func
	add rsp, 0x8
	jmp 1f

	0:
	call \func

	1:
	pop rsi
.endm


// gets: pointer to command buffer in rdi, pointer to memory buffer in rsi and
//       length of memory buffer in rdx.
// returns: pointer to error message in rax if an error occurred, otherwise 0 in rax.
brainfuck:
	push rbp
	mov rbp, rsp
	// r12: pointer to command buffer
	push r12
	mov r12, rdi
	// r13: memory buffer base pointer
	push r13
	// r14: memory buffer end pointer
	push r14
	// r15: EOF
	push r15
	mov r15, [rip+asm_eof]
	// rbx: stdin
	push rbx
	mov rbx, [rip+stdin]
	// save rdi
	push rdi
	// put "eof_behavior" on the stack
	mov rax, [rip+eof_behavior]
	push rax
	// put "memory_buf_limit" on the stack
	mov rax, [rip+memory_buf_limit]
	push rax
	// put "asm_bufsiz" on the stack
	mov rax, [rip+asm_bufsiz]
	push rax

	// rsi: working pointer for memory buffer

	// init memory buffer
	mov rdi, GET_ASM_BUFSIZ
	mov rsi, 1
	call s_calloc
	// base pointer
	mov r13, rax
	// working pointer
	mov rsi, r13
	// end pointer
	mov r14, r13
	add r14, GET_ASM_BUFSIZ


	.Loop:
	// get next character
	movzx rax, BYTE PTR [r12]
	test rax, rax
	jz .Lend

	// increment command pointer
	inc r12

	cmp rax, '>'
	je .Lforward
	cmp rax, '<'
	je .Lbackward
	cmp rax, '+'
	je .Lincrement
	cmp rax, '-'
	je .Ldecrement
	cmp rax, '.'
	je .Lwrite
	cmp rax, ','
	je .Lread
	cmp rax, '['
	je .Ljump_forward
	cmp rax, ']'
	jne .Loop // fall through


.Ljump_backward:
	cmp BYTE PTR [rsi], 0
	je .Loop
	// use bracket counter for nested loops
	mov rax, 0x1
	// revert command pointer increment
	dec r12

	.Ljump_backward_loop:
	// go one command back
	dec r12
	// verify that we still are within the command buffer
	cmp r12, GET_RDI
	jl .Ljump_backward_error

	// get command
	movzx rcx, BYTE PTR [r12]

	cmp rcx, '['
	je .Ljump_backward_found
	cmp rcx, ']'
	jne .Ljump_backward_loop
	inc rax
	jmp .Ljump_backward_loop

	.Ljump_backward_found:
	dec rax
	jnz .Ljump_backward_loop
	// set command pointer to command *after* matching '['
	inc r12
	jmp .Loop

	.Ljump_backward_error:
	movabsq rax, err_jump_backward
	jmp .Lend


.Ljump_forward:
	cmp BYTE PTR [rsi], 0
	jne .Loop
	// use bracket counter for nested loops
	mov rax, 0x1

	.Ljump_forward_loop:
	movzx rcx, BYTE PTR [r12]
	// set command pointer to command *after* matching ']'
	inc r12

	test rcx, rcx
	jz .Ljump_forward_error
	cmp rcx, ']'
	je .Ljump_forward_found
	cmp rcx, '['
	jne .Ljump_forward_loop
	inc rax
	jmp .Ljump_forward_loop

	.Ljump_forward_found:
	dec rax
	jnz .Ljump_forward_loop
	jmp .Loop

	.Ljump_forward_error:
	movabsq rax, err_jump_forward
	jmp .Lend


.Lforward:
	inc rsi
	// check if we still are within the allocated memory buffer
	cmp rsi, r14
	jl .Loop
	// if not, check if we may increase the memory buffer
	sub r14, r13 // end pointer (= working pointer) - base pointer
	mov rsi, r14
	cmp GET_MEMORY_BUF_LIMIT, rsi
	jbe .Loom
	// try to increase memory by BUFSIZ
	add rsi, GET_ASM_BUFSIZ
	// check if this is possible
	cmp GET_MEMORY_BUF_LIMIT, rsi
	jae .Lrealloc
	// if not, use memory buffer limit size as new size
	mov rsi, GET_MEMORY_BUF_LIMIT
	jmp .Lrealloc

	.Loom:
	movabsq rax, err_oom
	jmp .Lend


// expect: rsi = new memory buffer size; r14 = distance of old working pointer
// from r13
.Lrealloc:
	// rdi: base pointer
	mov rdi, r13
	// rsi will be preserved by "easy_call"
	easy_call s_realloc
	// update base pointer
	mov r13, rax
	// rdi: working pointer
	lea rdi, [r13+r14]
	push rdi
	// calculate end pointer
	lea r14, [r13+rsi]
	// rdx: size of new chunk
	mov rdx, r14
	sub rdx, rdi
	// rsi: 0
	xor rsi, rsi
	// zero new memory chunk
	easy_call memset
	// get working pointer
	pop rsi
	jmp .Loop


.Lbackward:
	dec rsi
	// check if we still are within the allocated memory buffer
	cmp rsi, r13
	jge .Loop
	movabsq rax, err_before_memory
	jmp .Lend


.Lincrement:
	inc BYTE PTR [rsi]
	jmp .Loop


.Ldecrement:
	dec BYTE PTR [rsi]
	jmp .Loop


.Lwrite:
	// rsi already is current pointer to memory
	movzx rdi, BYTE PTR [rsi]
	easy_call putchar
	cmp eax, r15d // EOF?
	jne .Loop
	movabsq rax, err_write
	jmp .Lend


// Problem: what to do in case of EOF?
// cf. https://en.wikipedia.org/wiki/Brainfuck#End-of-file_behavior
.Lread:
	easy_call getchar
	cmp eax, r15d // EOF?
	je .Leof
	mov BYTE PTR [rsi], al
	jmp .Loop

	.Leof:
	mov rax, GET_EOF_BEHAVIOR
	test rax, rax
	jnz 1f
	mov BYTE PTR [rsi], 0
	jmp 2f
	1:
	cmp rax, 1
	jne 2f
	mov BYTE PTR [rsi], -1
	2: // leave [rsi] unchanged

	.Lcheck_eof_error:
	mov rdi, rbx // load stdin
	easy_call ferror
	test eax, eax
	jz .Loop
	movabsq rax, err_read
	jmp .Lend


.Lend:
	// preserve possible error message in rax
	push rax
	// zero memory buffer
	mov rdi, r13
	xor rsi, rsi
	mov rdx, r14
	sub rdx, r13
	easy_call memset

	// free memory buffer
	mov rdi, r13
	easy_call free
	// restore rax
	pop rax

	add rsp, 0x20
	pop rbx
	pop r15
	pop r14
	pop r13
	pop r12
	pop rbp
	ret
