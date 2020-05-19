.intel_syntax noprefix
.global strlen_asm
.global strlen_asm_opt

# gets: pointer to string in rdi
# returns: string length in rax
strlen_asm:
	# save pointer
	lea rsi, [rdi]
	# use cl as zero
	xor rcx, rcx

	# do-while loop
	cmp BYTE PTR [rdi], cl
	jz .Lend

	.Loop:
	inc rdi
	cmp BYTE PTR [rdi], cl
	jnz .Loop

	.Lend:
	lea rax, [rdi]
	sub rax, rsi
	ret


# gets: pointer to string in rdi
# returns: string length in rax
strlen_asm_opt:
	# save pointer
	lea rsi, [rdi]

	# Make sure that the byte pointer is 8-Byte aligned (i.e. address%8 == 0)
	# before using it as QWORD pointer, thus preventing a possible page fault.
	# Thanks to my professor Martin Schulz (@TUM) for this nice idea!
	.Lalign:
	test rdi, 0x7
	jz .Lcontinue
	inc rdi
	cmp BYTE PTR [rdi-0x1], 0
	jnz .Lalign
	jmp .Lend_with_offset

	# let's go...
	.Lcontinue:
	# load constants
	mov r10, 0x0101010101010101
	mov r11, 0x8080808080808080

	.Loop_opt:
	mov rdx, QWORD PTR [rdi]
	lea rdi, [rdi+0x8]

	# check for null-byte - cf. https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
	lea r8, [rdx]
	sub rdx, r10
	not r8
	and r8, r11
	and rdx, r8
	jz .Loop_opt

	lea rdi, [rdi-0x8]
	# there is a null byte somwhere
	# process byte-for-byte to find it
	.Lfinish_the_rest:
	inc rdi
	cmp BYTE PTR [rdi-0x1], 0
	jnz .Lfinish_the_rest

	.Lend_with_offset:
	dec rdi
	.Lend_opt:
	lea rax, [rdi]
	sub rax, rsi
	ret
