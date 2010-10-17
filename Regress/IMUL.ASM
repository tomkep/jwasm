
;--- due to its 3 argument format,
;--- IMUL needs special treatment in the parser.

	.286
	.model small

	.data

vw	dw 0
vd	dd 0

	.code

	imul  ax,3
	imul  ax,1000
	imul  [vw]
	imul  [vwx]

	.386

	imul  eax,3
	imul  eax,1000
	imul  [vd]
	imul  [vdx]

	imul  ax,bx
	imul  eax,ebx
	imul  ax,[vw]
	imul  ax,[vwx]
	imul  eax,[vd]
	imul  eax,[vdx]
	imul  ax,[vw],2
	imul  ax,[vwx],2
	imul  eax,[vd],3
	imul  eax,[vdx],3
	ret

	.data

vwx	dw 0
vdx	dd 0

    END
