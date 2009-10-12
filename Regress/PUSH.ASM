
;--- test PUSH

	.model small
	.386

	.data

vw	dw     0
vsw SWORD  0
vd	dd     0
vsd SDWORD 0

	.code

main proc
	push ax
	push eax
	push [bx]
	push [ebx]
	push word ptr [ebx]
	push dword ptr [bx]
	push [bx][si+2]
	push [vw]
	push [vsw]
	push [vd]
	push [vsd]
	push word ptr [vd]
	pushf
	pushfd
	push 1
	push 7FFFFFFFh
	push 80000000h
	push 0FFFFFFFEh
	push -1
	push -32768
	push -65535
	push -65536
	push byte ptr 1
	push word ptr 2
	push dword ptr 4
	pushw 1
	pushw -1
	pushd 1
	pushd -1
	ret
main endp

    END
