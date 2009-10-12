
ifdef __JWASM__
	.x64
	.model flat
endif

	.data

vb	db 0
vw	dw 0
vd	dd 0
vq	dq 0

	.code

main proc
	imul  ax,bx
	imul  eax,ebx
	imul  rax,rbx
	imul  ax,3
	imul  eax,3
	imul  rax,3
	imul  ax,1000
	imul  eax,1000
	imul  rax,1000
	imul  ax,[vw]
	imul  eax,[vd]
	imul  rax,[vq]
	imul  [vw]
	imul  [vd]
	imul  [vq]
	imul  r11,5
	imul  r8,5
	imul  rsi,5
	ret
main endp

    END
