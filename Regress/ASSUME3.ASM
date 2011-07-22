
; jwasm prioritizes assumes differently from masm:
; masm enforces the assume on the LAST register
; jwasm enforces the assume on the FIRST register.

	.386
	.model flat

S1 struct
v1 dd ?
S1 ends

S2 struct
v2 dd ?
S2 ends

	.code

	assume eax:ptr S1
	assume edx:ptr S2
	mov [ecx+eax].v1,0	; Fails with jwasm < 2.06.
	mov [edx+eax].v1,0	; Fails with jwasm < 2.06.
;	mov [eax+edx].v1,0	; Works with jwasm < 2.06. Should fail.

	end
