
;--- DUP with initialized structures
;--- + macro in initializer

	.386
	.MODEL FLAT, stdcall
	option casemap:none

S1 struct
	db ?
S1 ends

TM1 macro
	E1=E1+1
	exitm <E1>
	endm

	.data

E1 = 0
	S1 300 dup (<LOW (TM1()+002+003)>)

	end
