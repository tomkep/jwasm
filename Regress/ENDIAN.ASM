
;--- test little-endian storage of string items

	.286
	.model tiny

	.data

	db "a"
	db 0
	db "abc"
	db 0
	db "abcd"
	db 0

	dw "ab"
	db 0
	dw "cd"
	db 0

	dd "abcd"
	db 0
	dd "abc"
	db 0

ifdef __JWASM__ ;Masm rejects
	df "abcdef"
	db 0
	df "abcde"
	db 0
endif

	dq "abcdefgh"
	db 0
	dq "abcde"
	db 0

;--- Masm 8/9 will define max 8 bytes for DT and OWORD
;--- (Masm 10 doesn't accept .TINY)

	dt "abcdefghij"
	db 0
	dt "abcdefg"
	db 0

	OWORD "abcdefghijklmnop"
	db 0
	OWORD "abcdefghijk"
	db 0

	end
