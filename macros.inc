; ------------------------------------------------------------------------------
; Macros for f3 
; ------------------------------------------------------------------------------

macro m_setTOS val
{
       mov TOS, val
}

macro m_push val
{
       ; inc [dDepth]
       add STKP, CELL_SIZE
       mov [STKP], TOS
       m_setTOS val
}

; ------------------------------------------------------------------------------
macro m_get2ND val
{
       mov val, [STKP]
}
macro m_set2ND val
{
       mov [STKP], val
}

; ------------------------------------------------------------------------------
macro m_getTOS val
{
       mov val, TOS
}

macro m_drop
{
       ; dec [dDepth]
       mov TOS, [STKP]
       sub STKP, CELL_SIZE
}

macro m_pop val
{
       m_getTOS val
       m_drop
}

macro m_pop2 val1, val2
{
       m_getTOS val1
       m_drop
       m_getTOS val2
       m_drop
}

; ------------------------------------------------------------------------------
macro m_toVmAddr reg
{
       add reg, edx
}

macro m_fromVmAddr reg
{
       sub reg, edx
}

; ------------------------------------------------------------------------------
macro m_rpush reg
{
       push TOS
       add [rStackPtr], CELL_SIZE
       mov TOS, [rStackPtr]
       mov [TOS], reg
       pop TOS
}
 
macro m_rpop reg
{
       push TOS
       mov TOS, [rStackPtr]
       mov reg, [TOS]
       sub [rStackPtr], CELL_SIZE
       pop TOS
}

; ------------------------------------------------------------------------------
macro m_CComma val
{
        m_push val
        call s_CComma
}

; ------------------------------------------------------------------------------
macro m_Comma val
{
        m_push val
        call s_Comma
}

; ------------------------------------------------------------------------------
macro c_CComma val
{
        db op_LIT1, val, op_CALL
        dd xtCComma
}

; ------------------------------------------------------------------------------
macro m_Save2 reg1, reg2
{
        push reg1
        push reg2
}

macro m_Restore2 reg1, reg2
{
        pop reg2
        pop reg1
}

; ------------------------------------------------------------------------------
macro m_Save3 reg1, reg2, reg3
{
        push reg1
        push reg2
        push reg3
}

macro m_Restore3 reg1, reg2, reg3
{
        pop reg3
        pop reg2
        pop reg1
}

; ------------------------------------------------------------------------------
macro m_NEXT
{
        movzx ecx, BYTE [PCIP]
        mov eax, [jmpTable+ecx*4]
        inc PCIP
        jmp eax
}

; ------------------------------------------------------------------------------
macro m_Define Tag, Flags, Length, Name, Last, xTag
{
    Tag: dd Last        ; Link
         dd xTag        ; XT
         db Flags       ; Flags
         db Length      ; Length
         db Name        ; Name
         db 0           ; NULL-terminator
    xTag:
}

; ------------------------------------------------------------------------------
macro m_DefinePrim Tag, Length, Name, Last, xTag, Prim
{
   Tag: dd Last        ; Link
        dd xTag        ; XT
        db INLINE      ; Flags
        db Length      ; Length
        db Name        ; Name
        db 0           ; NULL-terminator
  xTag: db Prim
        db op_EXIT
}

; ------------------------------------------------------------------------------
macro m_Lit1 val
{
    db op_LIT1, val
}

macro m_Lit4 val
{
    db op_LIT4
    dd val
}

macro m_Call val
{
    db op_CALL
    dd val
}

macro m_Exit
{
    db op_EXIT
}

; ------------------------------------------------------------------------------
