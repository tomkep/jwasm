
	.386
	.model flat

MemBlock struct
m_ptr DWORD ?
m_size DWORD ?
MemBlock ends

MemBlockPTR TYPEDEF PTR MemBlock

	.code

	ASSUME edx:MemBlockPTR, ecx:ptr MemBlock

mov eax,[edx].m_ptr
mov ebx,[ecx].m_size
	ret

	end
