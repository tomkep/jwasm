
_TEXT segment para 'code'
    jmp skip1
    test ax,ax
    jnz skip2
    db 126 dup (?)
skip1:
    ret
skip2:
    ret
_TEXT ends
    end
