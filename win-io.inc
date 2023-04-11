; I/O support for Windows
; ------------------------------------------------------------------------------

macro ioEMIT
{
    call [putchar]
    pop eax
}

macro ioKEY
{
    push ecx
    push edx
    call [_getch]
    pop edx
    pop ecx
    push eax
}

macro ioKEYq
{
    call [_kbhit]
    push eax
}

macro TRC ch
{
    push eax
    push ebx
    push ecx
    push edx
    push ch
    ioEMIT
    pop edx
    pop ecx
    pop ebx
    pop eax
}

macro ioACCEPT
{
    pop eax             ; ( buffer len--buffer )
    ; pop eax
    call [gets]
}

winTYPE:
type01: test edx, edx
        jz typeX
        movzx eax, BYTE [ecx]
        TRC eax
        inc ecx
        dec edx
        jmp type01
typeX:  ret

macro ioTYPE
{
    call winTYPE
}

macro ioTIMER
{
    call [GetTickCount]
    push eax
}

ioOpenFile: ret
ioCloseFile: ret
ioWriteFile: ret
