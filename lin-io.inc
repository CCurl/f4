; I/O support for Linux
; ------------------------------------------------------------------------------

; ( ch-- )
macro ioEMIT {
    pop eax
    mov	[buf4], al  ; put char in message
    mov	eax,4		; system call number (sys_write)
    mov	ebx,1		; file descriptor (stdout)
    mov	ecx,buf4    ; message to write
    mov	edx,1		; message length
    int	0x80		; call kernel
}

; ( addr len-- )
macro ioTYPE {
    pop edx         ; length
    pop ecx         ; message to write
    mov	eax,4		; system call number (sys_write)
    mov	ebx,1		; file descriptor (stdout)
    int	0x80		; call kernel
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

; ( addr sz--num )
macro ioACCEPT {
    pop edx         ; sz
    pop ecx         ; buffer
    push ecx
    mov	eax, 3      ; system call number (sys_read)
    mov	ebx, 0      ; file descriptor (stdin)
    int	0x80        ; call kernel
    pop ecx
    mov [ecx+eax-1], BYTE 0
    push eax        ; number of bytes read
}

; ( --ch )
macro ioKEY {
    push 0
}

; ( --flg )
macro ioKEYq {
    push 0
}

; ( --n )
macro ioTIMER {
    push 0
}
