.intel_syntax noprefix
.global brainfuck

.section .text
# gets: pointer to command buffer in rdi, pointer to memory buffer in rsi and
#       length of memory buffer in rdx.
# returns: pointer to error message in rax if an error occurred, otherwise 0 in rax.
brainfuck:
	# align stack and use rbx to save rdi
	push rbx
	mov rbx, rdi
	# use another register as pointer to command buffer
	mov r8, rdi
	# save rsi
	mov r9, rsi
	# calculate pointer to end of memory
	lea r10, [rsi+rdx]


	.Loop:
	# get next character
	movzx rax, BYTE PTR [r8]
	test rax, rax
	jz .Lend

	inc r8

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
	jne .Loop # fall through


.Ljump_backward:
	cmp BYTE PTR [rsi], 0
	je .Loop
	mov rax, 0x1 # use bracket counter
	dec r8

	.Ljump_backward_loop:
	dec r8
	cmp r8, rbx
	jl .Ljump_backward_error

	movzx rcx, BYTE PTR [r8]

	test rcx, rcx
	jz .Ljump_backward_error
	cmp rcx, '['
	je .Ljump_backward_found
	cmp rcx, ']'
	jne .Ljump_backward_loop
	inc rax
	jmp .Ljump_backward_loop

	.Ljump_backward_found:
	dec rax
	jnz .Ljump_backward_loop
	jmp .Loop

	.Ljump_backward_error:
	movabsq rax, err_jump_backward
	jmp .Lend


.Lforward:
	inc rsi
	cmp rsi, r10
	jl .Loop
	movabsq rax, err_oom
	jmp .Lend


.Lbackward:
	dec rsi
	cmp rsi, r9
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
	mov rax, 0x1 # write syscall
	mov rdi, 0x1 # stdout
	# rsi already is current pointer to memory
	mov rdx, 0x1 # write one byte
	syscall
	cmp rax, rdx
	je .Loop
	movabsq rax, err_write
	jmp .Lend


.Lread:
	xor rax, rax # read syscall
	xor rdi, rdi # stdin
	# rsi already is current pointer to memory
	mov rdx, 0x1 # read one byte
	syscall
	cmp rax, rdx
	je .Loop
	cmp rax, 0
	jl .Lread_err
	movabsq rax, stdin_eof
	jmp .Lend
	.Lread_err:
	movabsq rax, err_read
	jmp .Lend


.Ljump_forward:
	cmp BYTE PTR [rsi], 0
	jne .Loop
	mov rax, 0x1 # use bracket counter

	.Ljump_forward_loop:
	movzx rcx, BYTE PTR [r8]
	inc r8

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
	# fall through


.Lend:
	pop rbx
	ret
