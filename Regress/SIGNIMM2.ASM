
;--- arithmetic op with signed op and forward memory reference

_text segment 'CODE'

	assume ds:_data

	cmp digb,255
	cmp digb,-1
	cmp digw,255
	cmp digw,256
	cmp digw,32768
	cmp digw,65535
	cmp digw,-1
	cmp digw,word ptr -1

_text ends

_data segment 'DATA'
digb db 0
digw dw 0
_data ends

	End
