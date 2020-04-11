; Some pitfalls I stumbeled about:
; https://robertimschweiler.com/notes_64bit_intel_assembly.html

; extern declarations
extern fdopen
extern fflush
extern printf
extern exit


section .bss
	string: RESB 100000000 ; 100 MB, 100.000.000 bytes
	strlen_cmd: RESQ 1


section .data
	; format string for printf to print the value of a 64-bit register as
	; unsigned decimal
	format_string_error: db "An error occurred - errno is: %d", 0xa, 0x0
	format_string_ureg: db "%llu", 0xa, 0x0
	newline: db 0xa, 0x0
	read_mode: db "r", 0x0
	strlen_str: db "strlen", 0x0
	strlen_opt4_str: db "strlen_opt4", 0x0
	strlen_opt8_str: db "strlen_opt8", 0x0
	usage_str: db "usage: strlen_asm [STRLEN_CMD]", 0xa, 0xa, \
		"STRLEN_CMD may be:", 0xa, \
		"  strlen (normal strlen implementation)", 0xa, \
		"  strlen_opt4 (operates on DWORD string)", 0xa, \
		"  strlen_opt8 (operates on QWORD string)", 0xa, \
		0x0


section .text

;;;;;;;;;;
; macros ;
;;;;;;;;;;

; Note: The stack has to be 16-byte-aligned -> 16n+8-byte aligned before call
; as call pushes an additional 8-byte value.
; I think that Macros should consider that!

; print a newline to stdout
%macro NEWLINE 0
	push rdi
	sub rsp, 0x8 ; align

	mov rdi, newline
	call print_str ; result may be checked by the caller

	add rsp, 0x8 ; revert alignment
	pop rdi
%endmacro


;;;;;;;;;;;;;;;;;;;;
; helper functions ;
;;;;;;;;;;;;;;;;;;;;

; print error message to stdout and exit; errno is in rdi
error:
	mov rsi, rdi

	xor rax, rax
	mov rdi, format_string_error

	call printf ; result may be checked by the caller

	mov rsi, 1 ; EXIT_FAILURE
	call exit

;%macro FLUSH 0
;	push rdi
;	push rsi
;
;	mov rdi, 1 ; stdout
;	mov rsi, read_mode
;	call fdopen
;	; FILE pointer should be in rax
;	mov rdi, rax
;	call fflush
;
;	pop rsi
;	pop rdi
;%endmacro


; write string (pointer to string in rdi) to stdout
print_str:
	push rdi
	push rsi
	sub rsp, 0x8 ; align

	mov rsi, rdi

	; get string length (last argument to write())
	sub rsp, 0x8 ; align - one push following
	push rsi
	call strlen
	mov rdx, rax ; move result to rdx
	add rsp, 0x10 ; clean stack

	mov rax, 1 ; write
	mov rdi, 1 ; stdout
	; rsi already contains pointer to string
	; rdx already contains strlen

	syscall ; result may be checked by the caller

	add rsp, 0x8 ; revert alignment
	pop rsi
	pop rdi
	ret


; print content of rdi as unsigned decimal
print_ureg:
	push rdi
	push rsi
	sub rsp, 0x8 ; align

	mov rsi, rdi

	xor rax, rax ; rax needs to be zeroed for printf
	mov rdi, format_string_ureg

	call printf ; result may be checked by the caller

	add rsp, 0x8 ; revert alignment
	pop rsi
	pop rdi
	ret


; Read max. "rsi"-1 bytes from stdin into buf pointed to by rdi and append a
; null-byte.
; (Multiple null-bytes may be read into buf. So strlen actually is the number
; of bytes till the *first* null-byte.)
read_stdin:
	push rdi
	push rsi
	sub rsp, 0x8 ; align

	mov rdx, rsi ; max
	dec rdx ; read one byte less
	mov rsi, rdi ; pointer to buf

	mov rax, 0 ; read
	mov rdi, 0 ; stdin

	syscall ; result may be checked by the caller

	; append null byte
	mov BYTE [rsi+rax], 0

	add rsp, 0x8 ; revert alignment
	pop rsi
	pop rdi
	ret


;;;;;;;;;;;;;
; functions ;
;;;;;;;;;;;;;


global main
main:
	push rbp
	mov rbp, rsp
	push rdi
	push rsi

	; check argc
	cmp rdi, 2
	jne .usage

	add rsi, 8

	; check if argv[1] == strlen_str || argv[1] == strlen_opt_str
	push QWORD [rsi]
	push strlen_str
	call strcmp
	add rsp, 0x10 ; remove pointer from stack
	test rax, rax
	jz .strlen_is_strlen_str

	push QWORD [rsi]
	push strlen_opt4_str
	call strcmp
	add rsp, 0x10 ; remove pointer from stack
	test rax, rax
	jz .strlen_is_strlen_opt4_str

	push QWORD [rsi]
	push strlen_opt8_str
	call strcmp
	add rsp, 0x10 ; remove pointer from stack
	test rax, rax
	jz .strlen_is_strlen_opt8_str

	jmp .usage

	.strlen_is_strlen_str:
	mov QWORD [strlen_cmd], strlen
	jmp .body

	.strlen_is_strlen_opt4_str:
	mov QWORD [strlen_cmd], strlen_opt4
	jmp .body

	.strlen_is_strlen_opt8_str:
	mov QWORD [strlen_cmd], strlen_opt8

	.body:

	; get input
	mov rdi, string
	mov rsi, 100000000
	call read_stdin

	sub rsp, 0x8 ; align stack - one push following
	push string ; put pointer to string on the stack

	call QWORD [strlen_cmd] ; result will be in rax

	add rsp, 0x10 ; clean stack

	mov rdi, rax
	call print_ureg ; print result

	xor rax, rax ; EXIT_SUCCESS
	jmp .end

	.usage:
	mov rdi, usage_str
	call print_str
	mov rax, 1 ; EXIT_FAILURE

	.end:
	pop rsi
	pop rdi
	pop rbp
	ret


; place result in rax
; strcmp could be optimised, too :-)
strcmp:
	push rbp
	mov rbp, rsp
	push rbx
	push rsi

	xor rax, rax
	xor rbx, rbx
	xor rsi, rsi

	; get address of first string
	mov rcx, [rbp+16]
	; get address of second string
	mov rdx, [rbp+24]

	.loop:

	mov al, BYTE [rcx+rsi]
	mov bl, BYTE [rdx+rsi]
	inc rsi

	test al, al
	jz .end
	test bl, bl
	jz .end

	cmp al, bl
	jz .loop

	.end:
	sub al, bl

	pop rsi
	pop rbx
	pop rbp
	ret


; place result in rax
strlen:
	push rbp
	mov rbp, rsp

	; get pointer to string from the stack
	mov rdx, [rbp+16]
	xor rax, rax
	xor rcx, rcx ; use rcx as zero

	; do-while loop
	cmp BYTE [rdx], cl
	jz .end

	.loop:
	inc rax
	cmp BYTE [rdx+rax], cl
	jnz .loop

	.end:
	pop rbp
	ret


; place result in rax
strlen_opt4:
	push rbp
	mov rbp, rsp
	push rbx
	push rsi
	push rdi
	sub rsp, 0x8 ; align stack

	mov rsi, [rbp+16] ; load pointer to string
	xor rax, rax

	; make sure that the string is 4-byte-aligned
	call alignment_check
	test rax, rax
	jnz .finished

	; load constants
	mov rcx, 0x00ff0000
	mov rdx, 0xff000000
	mov rdi, 4

	.loop:
	; get current "foursome" of characters
	mov ebx, [rsi+rax]
	; check byte-for-byte
	test bl, bl
	jz .byte0_zero
	test bh, bh
	jz .byte1_zero
	test ebx, ecx
	jz .byte2_zero
	test ebx, edx
	jz .byte3_zero
	add rax, rdi ; increment counter
	jmp .loop

	.byte0_zero:
	jmp .end
	.byte1_zero:
	add rax, 1
	jmp .end
	.byte2_zero:
	add rax, 2
	jmp .end
	.byte3_zero:
	add rax, 3
	jmp .end

	.finished:
	mov rax, rcx

	.end:
	add rsp, 0x8 ; revert alignment
	pop rdi
	pop rsi
	pop rbx
	pop rbp

	ret


; place result in rax
strlen_opt8:
	push rbp
	mov rbp, rsp
	push rbx
	push rsi
	push rdi
	sub rsp, 0x8 ; align

	mov rsi, [rbp+16] ; load pointer to string
	xor rax, rax ; reset rax

	; make sure that the string is 8-byte-aligned
	call alignment_check
	test rax, rax
	jnz .finished

	; load constants
	mov rcx, 0x0000000000ff0000
	mov rdx, 0x00000000ff000000
	mov r8,  0x000000ff00000000
	mov r9,  0x0000ff0000000000
	mov r10, 0x00ff000000000000
	mov r11, 0xff00000000000000
	mov rdi, 4

	.loop:
	; get current "foursome" of characters
	mov rbx, [rsi+rax]
	; check byte-for-byte
	test bl, bl
	jz .byte0_zero
	test bh, bh
	jz .byte1_zero
	test rbx, rcx
	jz .byte2_zero
	test rbx, rdx
	jz .byte3_zero
	test rbx, r8
	jz .byte4_zero
	test rbx, r9
	jz .byte5_zero
	test rbx, r10
	jz .byte6_zero
	test rbx, r11
	jz .byte7_zero
	add rax, rdi ; increment counter
	jmp .loop

	.byte0_zero:
	jmp .end
	.byte1_zero:
	add rax, 1
	jmp .end
	.byte2_zero:
	add rax, 2
	jmp .end
	.byte3_zero:
	add rax, 3
	jmp .end
	.byte4_zero:
	add rax, 4
	jmp .end
	.byte5_zero:
	add rax, 5
	jmp .end
	.byte6_zero:
	add rax, 6
	jmp .end
	.byte7_zero:
	add rax, 7
	jmp .end

	.finished:
	mov rax, rcx

	.end:
	add rsp, 0x8 ; revert alignment
	pop rdi
	pop rsi
	pop rbx
	pop rbp

	ret


; Make sure that the byte pointer is "rax"-Byte aligned (i.e. address%rax == 0)
; before casting it to a DWORD/QWORD pointer, thus preventing a possible page
; fault.
; Thanks to my professor Martin Schulz (@TUM) for this nice idea!
;
; arguments:
; alignment factor (4 or 8) in rax, string in rsi
;
; return values:
; Return 1 in rax and string length in rcx if null-byte has already been
; found within possible offset. Otherwise, rax and rcx will be zero.
; Return aligned pointer to string in rsi.
alignment_check:
	push rbp
	mov rbp, rsp

	mov rcx, rsi
	mov rdx, rax
	dec rdx

	and rcx, rdx ; check if the relevant first bits of the pointer are set
	jz .no_offset ; if not, everything is fine

	; if yes, we have to check if the terminating null-byte is within
	; the first rax-rcx bytes
	sub rax, rcx
	xor r8, r8

	.loop:
	cmp BYTE [rsi+r8], 0
	jz .null_byte_found
	inc r8
	cmp r8, rax
	jnz .loop

	; we do not already know the length of the string, but any
	; further processing must start with the aligned string
	xor rax, rax
	xor rcx, rcx
	add rsi, r8
	jmp .end

	.null_byte_found:
	mov rax, 1
	mov rcx, r8
	jmp .end

	.no_offset:
	xor rax, rax
	xor rcx, rcx

	.end:
	pop rbp
	ret
