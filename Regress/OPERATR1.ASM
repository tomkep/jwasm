
;--- operators SIZEOF, LENGTHOF, SIZE, LENGTH, TYPE

_DATA segment word public 'DATA'

vb1	db 1
vb2	db 1,2,3
vb3	db "abc"
vb4	db "abc","def"
vb5	db 4 dup (3), 2 dup (4)
	dw vb1, vb2, vb3, vb4
	db sizeof vb1, lengthof vb1, size vb1, length vb1, type vb1
	db sizeof vb2, lengthof vb2, size vb2, length vb2, type vb2
	db sizeof vb3, lengthof vb3, size vb3, length vb3, type vb3
	db sizeof vb4, lengthof vb4, size vb4, length vb4, type vb4
	db sizeof vb5, lengthof vb5, size vb5, length vb5, type vb5
	dw opattr vb1, .type vb1

_DATA ends

	END
