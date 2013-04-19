
	.386
	.model flat

	.code

	dw opattr [ebp+ecx]		;results: ml=22h; jwasm=62h ( 40h=stack related )

p1 proc
local x1[2]:byte

	dw opattr [x1+ecx]		;result: 62h
	dw opattr (x1[ecx])		;result: 62h

	dw opattr [ecx+x1]		;result: 62h
	dw opattr ([ecx]x1)		;results: ml=0; jwasm v.210=0; jwasm < 2.10: 'missing operator in expression'

p1 endp

END

