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
    ;pop edx         ; length
    ;pop ecx         ; message to write
    mov	eax,4		; system call number (sys_write)
    mov	ebx,1		; file descriptor (stdout)
    int	0x80		; call kernel
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

; -------------------------------------------------------------------------------------
; ioOpenFile: Open a file
;   Params:  EBX: FileName, ECX: Flags, EDX: Mode
;   Returns: EAX: FileHandle
ioOpenFile:
    mov	eax,5		; system call number (sys_open)
    int	0x80		; call kernel
    ret

; -------------------------------------------------------------------------------------
; ioOpenFile: Close a file
;   Params:  EBX: FileHandle
;   Returns: None
ioCloseFile:
    mov	eax,6		; system call number (sys_close)
    int	0x80		; call kernel
    ret

; -------------------------------------------------------------------------------------
; ioWriteFile: Write to a file
;   Params:  EBX: FileHandle, ECX: buffer, EDX: length
;   Returns: None
ioWriteFile:
    mov	eax,4		; system call number (sys_write)
    int	0x80		; call kernel
    ret
