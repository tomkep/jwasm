
;--- INVOKE with FASTCALL, 32-bit

	.386
	.model flat

	.code

fast1 proc fastcall a1:dword
	mov eax,a1
	ret
fast1 endp

fast2 proc fastcall a1:dword, a2:word
	mov eax,a1
	add eax,a2
	ret
fast2 endp

fast3 proc fastcall a1:dword, a2:byte, a3:dword
	mov eax,a1
	add eax,a2
	add eax,a3
	ret
fast3 endp

fast4 proc fastcall a1:dword, a2:byte, a3:dword, a4:dword
	mov eax,a1
	add eax,a2
	add eax,a3
	add eax,a4
	ret
fast4 endp

start:
	invoke fast1, 1
	invoke fast2, 1, 2
	invoke fast3, 1, 2, 3
	invoke fast4, 1, 2, 3, 4
	mov ah,4Ch
	int 21h

end
