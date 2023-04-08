; **************************************************************************
;
; This is a program that implements a Forth CPU/VM.
; The VM's memory space is the reserved buffer at THE_MEMORY.
;
; **************************************************************************
;                 x86 Register usage
; **************************************************************************
;
IP   equ esi       ; Program-Counter/Instruction-Pointer
SP   equ esp       ; Stack-Pointer
RSP  equ ebp       ; Return Stack-Pointer
TOS  equ [esp]     ; Top Of Stack
RTOS equ [ebp]     ; Top Of Return Stack
CELL_SIZE = 4
;
; **************************************************************************

; FOR_OS equ WINDOWS
; FOR_OS equ LINUX

match =WINDOWS, FOR_OS { include 'win-su.inc' }
match =LINUX,   FOR_OS { include 'lin-su.inc' }

; -------------------------------------------------------------------------------------
macro NEXT
{
       lodsd
       jmp DWORD [eax]
}

; ------------------------------------------------------------------------------
macro rPUSH val
{
    add RSP, CELL_SIZE
    mov RTOS, DWORD val
}

; ------------------------------------------------------------------------------
macro rPOP val
{
    mov val, DWORD RTOS
    sub RSP, CELL_SIZE
}

; ------------------------------------------------------------------------------
macro DefWord Name, Length, Flags, Tag
{
        d_#Tag: dd LastTag     ; Link
                dd Tag         ; XT / Code-Field-Address (CFA)
                db Flags       ; Flags
                db Length      ; Length
                db Name        ; Name
                db 0           ; NULL-terminator
        align CELL_SIZE
        LastTag equ d_#Tag
        Tag:    dd  DOCOL
}

; ------------------------------------------------------------------------------
macro DefVar Name, Length, Flags, Tag
{
        d_#Tag: dd LastTag      ; Link
                dd Tag          ; XT / Code-Field-Address (CFA)
                db Flags        ; Flags
                db Length       ; Length
                db Name         ; Name
                db 0            ; NULL-terminator
        align CELL_SIZE
        LastTag equ  d_#Tag
        Tag:    dd   c_#Tag
        c_#Tag: push v_#Tag
        NEXT
}

; ------------------------------------------------------------------------------
macro DefCode Name, Length, Flags, Tag
{
        d_#Tag: dd LastTag     ; Link
                dd Tag         ; XT / Code-Field-Address (CFA)
                db Flags       ; Flags
                db Length      ; Length
                db Name        ; Name
                db 0           ; NULL-terminator
        align CELL_SIZE
        LastTag equ  d_#Tag
        Tag:    dd   c_#Tag
        c_#Tag:
}

; ------------------------------------------------------------------------------
LastTag equ 0

MEM_SZ    equ 4*1024*1024
IMMEDIATE equ 1
INLINE    equ 2

; -------------------------------------------------------------------------------------
match =WINDOWS, FOR_OS { include 'win-io.inc' }
match =LINUX,   FOR_OS { include 'lin-io.inc' }

; -------------------------------------------------------------------------------------
; -------------------------------------------------------------------------------------
; -------------------------------------------------------------------------------------
match =WINDOWS, FOR_OS { section '.code' code readable executable }
match =LINUX,   FOR_OS { segment readable executable }

; -------------------------------------------------------------------------------------
; -------------------------------------------------------------------------------------
; -------------------------------------------------------------------------------------
entry $
        mov [InitialESP], esp
        mov [InitialEBP], ebp
        cld

        mov RSP, rStack
        mov esi, coldStart
        NEXT

; -------------------------------------------------------------------------------------
coldStart:
        dd QUIT

; -------------------------------------------------------------------------------------
DOCOL:
        rPUSH esi           ; push current esi on to the return stack
        add eax, CELL_SIZE  ; eax points to codeword, so add (CELL_SIZE) to make
        mov esi, eax        ; esi point to first data word
        NEXT

; -------------------------------------------------------------------------------------
DefCode "EXIT",4,0,EXIT
        rPOP esi            ; get esi back
        NEXT

; -------------------------------------------------------------------------------------
DefCode "0RSP",4,0,zRSP
        mov RSP, rStack         ; Reset the return stack
        mov [lSP], lStack       ; Reset the loop stack too
        cmp esp, [InitialESP]   ; Reset the data stack if underflow
        jle rs0X
        mov esp, [InitialESP]
rs0X:   NEXT

; -------------------------------------------------------------------------------------
DefCode "0SP",3,0,zSP
        mov esp, [InitialESP]
        NEXT

; -------------------------------------------------------------------------------------
DefCode "SP0",3,0,SP0
        push DWORD [InitialESP]
        NEXT

; -------------------------------------------------------------------------------------
; Comma: The standard FORTH ','
;       Params: EAX => val
Comma:  push edx
        mov edx, [v_HERE]
        mov [edx], eax
        add edx, CELL_SIZE
        mov [v_HERE], edx
        pop edx
        ret

DefCode ",",1,0,COMMA
        pop eax
        call Comma
        NEXT

; -------------------------------------------------------------------------------------
DefCode "C,",2,0,CCOMMA
        pop eax
        mov edx, [v_HERE]
        mov [edx], eax
        inc edx
        mov [v_HERE], edx
        NEXT

; -------------------------------------------------------------------------------------
toLower: ; Make DL lower-case if between A-Z
        cmp dl, 'A'
        jl tlX
        cmp dl, 'Z'
        jg tlX
        add dl, 0x20
tlX:    ret

; -------------------------------------------------------------------------------------
; strEqI: Case-insensitive string-equals.
;         Params: string 1: EAX/EBX (string/len)
;                 string 2: ECX/EDX (string/len)
;         Return: EAX=0 => Not equal, or EAX=1 => equal
strEqI: cmp ebx, edx            ; Are the lengths the same?
        jne eqiNo
eqi01:  test ebx, ebx           ; No more chars?
        jz eqiYes
        mov dl, [eax]           ; char1
        call toLower
        mov dh, dl
        mov dl, [ecx]           ; char 2
        call toLower
        cmp dl, dh
        jne eqiNo
        inc eax                 ; So far, so good ... next chars
        inc ecx
        dec ebx
        jmp eqi01
eqiNo:  xor eax, eax            ; Strings are not equal
        ret
eqiYes: mov eax, 1              ; Strings are equal
        ret

; -------------------------------------------------------------------------------------
; StrEqI ( c-str c-str2 -- f ) - Case Insensitive compare
;       f == 0, NOT equals, f != 0, equals
DefCode "StrEqI",6,0,STREQI
        pop eax
        pop ecx
        movzx ebx, BYTE [eax]
        movzx edx, BYTE [ecx]
        inc eax
        inc ecx
        call strEqI
        push eax
        NEXT

; -------------------------------------------------------------------------------------
; doFind: Version of FIND that can be called from assembly
;       Params: ECX: counted string
;       Return: If found, EAX=[entry]
;               If not found, EAX=0
;               NOTE: EBX is used/destroyed
doFind: movzx edx, BYTE [ecx]   ; EDX: len
        inc ecx                 ; ECX: string
        mov eax, [v_LAST]       ; EAX: the current dict entry
fw01:   test eax, eax           ; end of dictionary?
        jz fwX
        push edx                ; Save length
        push ecx                ; Save string
        push eax                ; Save the current word
        add eax, CELL_SIZE*2+1  ; add length offset
        movzx ebx, BYTE [eax]   ; length2
        inc eax                 ; string2
        call strEqI             ; Return is in EAX
        pop ebx                 ; Get current dict entry (was EAX)
        pop ecx                 ; Get string back
        pop edx                 ; Get length back
        test eax, eax           ; EAX != 0 means they are equal
        jnz fwY
        mov eax, [ebx]          ; Not equal, move to the next word
        jmp fw01
fwY :   mov eax, ebx
fwX :   ret

; -------------------------------------------------------------------------------------
; FIND: Look for a word in the dictionary.
;       Stack: if found,     ( c-addr -- xt 1 | xt -1 )
;              if not found, ( c-addr -- c-addr 0 ) 
DefCode "FIND",4,0,FIND
        mov ecx, DWORD TOS      ; Input string, leave on stack
        call doFind
        test eax, eax
        jnz fndY
        push eax                ; 0 means not found
        NEXT
fndY:   pop ebx                 ; Discard input string
        add eax, CELL_SIZE      ; XT
        push DWORD [eax]
        add eax, CELL_SIZE      ; 1 means IMMEDIATE, else -1
        movzx ecx, BYTE [eax]
        cmp ecx, IMMEDIATE
        je fndX
        dec ecx
fndX:   push ecx
        NEXT

; -------------------------------------------------------------------------------------
DefCode "EXECUTE",7,0,EXECUTE           ; ( xt-- )
        pop eax
        jmp DWORD [eax]

; -------------------------------------------------------------------------------------
; doCStr: Parse the next word by delim in  into curWord as a counted string.
;       Params: BL  => Delimiter (0 means whitespace)
;       Return: ECX => length of word
;       NOTE: ECX=0 means end of line
doCStr: mov edi, curWord
        mov edx, [curIn]
        xor ecx, ecx            ; length is 0
        xor eax, eax
        stosb                   ; 1st byte => length 0
        cmp bl, 32              ; Skip WS only if bl == 32
        jne cs01
csSKW:  mov al, [edx]           ; Get the current char
        test al, al             ; EOL?
        jz csX
        cmp al, 32
        jg cs01
        inc edx
        jmp csSKW
cs01:   mov al, [edx]           ; Get the current char
        jz csX                  ; EOL?
        cmp al, bl              ; Hit the delim?
        je csX
        cmp bl, 32              ; WS delimiter needs more checking
        jne csCW
        cmp al, 33
        jl csX
csCW:   stosb                   ; Collect the char into curWord
        inc ecx                 ; update length
        inc edx                 ; Next char
        jmp cs01
csX:    mov [curWord], cl
        mov [curIn], edx
        mov [edi], BYTE 0       ; Add NULL terminator
        ret

; -------------------------------------------------------------------------------------
DefCode "WORD",5,0,xtWORD     ; ( --cStr )
        mov bl, 32
        call doCStr
        push DWORD curWord
        NEXT

; -------------------------------------------------------------------------------------
DefCode "ACCEPT",6,0,ACCEPT     ; ( addr sz--num )
        ioACCEPT
        NEXT

; -------------------------------------------------------------------------------------
DefCode "BRANCH",6,0,BRANCH
        lodsd
        mov esi, eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "0BRANCH",7,0,zBRANCH
        lodsd
        pop edx
        test edx, edx
        jnz zBX
        mov esi, eax
zBX:    NEXT

; -------------------------------------------------------------------------------------
DefCode "?BRANCH",7,0,nzBRANCH
        lodsd
        pop edx
        test edx, edx
        jz nzBX
        mov esi, eax
nzBX:   NEXT

; -------------------------------------------------------------------------------------
DefCode "DUP",3,0,DUP1
        mov eax, TOS
        push eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "?DUP",4,0,qDUP
        cmp TOS, DWORD 0
        jnz c_DUP1
        NEXT

; -------------------------------------------------------------------------------------
DefCode "2DUP",4,0,DUP2
        pop ebx
        pop eax
        push eax
        push ebx
        push eax
        push ebx
        NEXT

; -------------------------------------------------------------------------------------
DefCode "SWAP",4,0,SWAP
        pop eax
        pop ebx
        push eax
        push ebx
        NEXT

; -------------------------------------------------------------------------------------
DefCode "OVER",4,0,OVER
        pop ebx
        mov eax, TOS
        push ebx
        push eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "NIP",3,0,NIP
        pop ebx
        pop eax
        push ebx
        NEXT

; -------------------------------------------------------------------------------------
DefCode "DROP",4,0,DROP
        pop eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "2DROP",5,0,DROP2
        pop eax
        pop eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "LIT",3,0,LIT
        lodsd
        push eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "1+",2,0,INCTOS
        inc DWORD TOS
        NEXT

; -------------------------------------------------------------------------------------
DefCode "1-",2,0,DECTOS
        dec DWORD TOS
        NEXT

; -------------------------------------------------------------------------------------
DefCode "@",1,0,FETCH
        mov edx, TOS
        mov eax, [edx]
        mov TOS, eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "C@",2,0,CFETCH
        mov edx, TOS
        movzx eax, BYTE [edx]
        mov TOS, eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "!",1,0,fSTORE
        pop edx
        pop eax
        mov [edx], eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "C!",2,0,CSTORE
        pop edx
        pop eax
        mov [edx], al
        NEXT

; -------------------------------------------------------------------------------------
DefCode "+",1,0,xtADD
        pop edx
        pop eax
        add eax, edx
        push eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "-",1,0,xtSUB
        pop edx
        pop eax
        sub eax, edx
        push eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "*",1,0,xtMUL
        pop edx
        pop eax
        mul edx
        push eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "/",1,0,xtDIV
        pop ebx
        pop eax
        xor edx, edx
        idiv ebx
        push eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "<",1,0,LESSTHAN
        pop ebx
        pop eax
        cmp eax, ebx
        jl fTrue
        jmp fFalse

; -------------------------------------------------------------------------------------
DefCode "=",1,0,EQUALS
        pop ebx
        pop eax
        cmp eax, ebx
        je fTrue
fFalse: push 0
        NEXT
fTrue:  push -1
        NEXT

; -------------------------------------------------------------------------------------
DefCode ">",1,0,GREATERTHAN
        pop ebx
        pop eax
        cmp eax, ebx
        jg fTrue
        jmp fFalse

; -------------------------------------------------------------------------------------
DefCode "/MOD",4,0,xSLMOD
        pop ebx
        pop eax
        xor edx, edx
        idiv ebx
        push edx
        push eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "KEY",3,0,KEY         ; ( ch-- )
        ioKEY
        NEXT

; -------------------------------------------------------------------------------------
DefCode "KEY?",4,0,KEYQ         ; ( ch-- )
        ioKEYq
        NEXT

; -------------------------------------------------------------------------------------
DefCode "EMIT",4,0,EMIT         ; ( ch-- )
        ioEMIT
        NEXT

; -------------------------------------------------------------------------------------
DefCode "COUNT",5,0,COUNT       ; ( c-str--addr len )
        pop edx
        movzx ecx, BYTE [edx]
        inc edx
        push edx
        push ecx
        NEXT

; -------------------------------------------------------------------------------------
DefCode "TYPE",4,0,TYPE         ; ( addr len-- )
        ioTYPE
        NEXT

; -------------------------------------------------------------------------------------
DefCode "TIMER",5,0,TIMER       ; ( --n )
        ioTIMER
nxt:    NEXT

; -------------------------------------------------------------------------------------
; doCreate: Params: none, uses curWord
doCreate:
        mov edx, curWord
        mov cl, [edx]
        inc edx
        mov edi, [v_HERE]               ; Use EDI as HERE
        mov eax, [v_LAST]
        mov [edi], eax                  ; Link
        mov [v_LAST], edi               ; Update LAST
        add edi, CELL_SIZE
        push edi                        ; FWD ref for the XT
        add edi, CELL_SIZE
        mov [edi], BYTE 0               ; Flags
        inc edi
        mov [edi], cl                   ; Length
        inc edi
crt01:  test cl, cl                   ; word-name
        jz crt02
        mov al, [edx]
        stosb
        inc edx
        dec cl
        jmp crt01
crt02:  mov [edi], BYTE 0               ; null terminator
crtA:   test edi, CELL_SIZE-1           ; align
        jz crt03
        inc edi
        jmp crtA
crt03:  pop eax                         ; resolve XT fwd ref
        mov [eax],edi
        mov [v_HERE], edi
        ret

; -------------------------------------------------------------------------------------
DefCode "CREATE",6,0,CREATE
        mov bl, 32
        call doCStr
        call doCreate
        NEXT

; -------------------------------------------------------------------------------------
; digitQ: Set AH=0 if AL is a digit in base BL, else AH=1.
;           NOTE: if AH=0, AL will be the converted digit.
digitQ: cmp al, '0'                     ; <'0' means no
        jl dqNo
        sub al, '0'                     ; Convert to number
        cmp al, bl                      ; 0 .. (base-1) => OK
        jl dqYes
        cmp bl, 10                      ; BASE>10 needs more checking
        jle dqNo
        sub al, 7                       ; Hex: 'A'-'0'-7 => 10
        cmp al, 9
        jle dqNo
        cmp al, bl
        jge dqNo
dqYes:  mov ah, 0
        ret
dqNo:   mov ah, 1
        ret

; -------------------------------------------------------------------------------------
DefCode "WORDS",5,0,WORDS
        mov eax, [v_LAST]       ; EAX: the current dict entry
ww01:   test eax, eax           ; end of dictionary?
        jz nxt
        push eax                ; Save the current word
        add eax, CELL_SIZE*2+1  ; add length offset
        movzx ebx, BYTE [eax]   ; length
        inc eax                 ; string
        push eax
        push ebx
        ioTYPE
        push 9
        ioEMIT
        pop ebx                 ; Get addr back
        mov eax, [ebx]          ; Move to the next word
        jmp ww01

; -------------------------------------------------------------------------------------
DefCode "DO", 2, 0, xtDO
        mov edx, [lSP]
        pop ecx                 ; From / I
        pop ebx                 ; To
        add edx, CELL_SIZE
        mov [edx], DWORD esi    ; loop start
        add edx, CELL_SIZE
        mov [edx], DWORD ebx
        add edx, CELL_SIZE
        mov [edx], DWORD ecx
        mov [lSP], DWORD edx
        NEXT

; -------------------------------------------------------------------------------------
DefCode "I", 1, 0, I
        mov edx, [lSP]
        push DWORD [edx]
        NEXT

; -------------------------------------------------------------------------------------
DefCode "LOOP", 4, 0, xtLOOP
        mov edx, [lSP]
        inc DWORD [edx]                         ; I
        mov ecx, DWORD [edx]
        cmp ecx, DWORD [edx-CELL_SIZE]          ; TO
        jge lpDone
        mov esi, DWORD [edx-CELL_SIZE*2]        ; Loop start
        NEXT
lpDone: sub edx, CELL_SIZE*3
        mov [lSP], DWORD edx
        NEXT

; -------------------------------------------------------------------------------------
DefCode "<#", 2, 0, DOTinit             ; ( n--u )
        mov [isNeg], BYTE 0
        mov [dotLen], BYTE 0
        mov eax, dotBuf+63
        mov [dotPtr], eax
	pop eax                         ; Check if negative
	test eax, eax
	jns di99
	cmp [v_BASE], DWORD 10          ; Only for base 10
	jne di99
	mov [isNeg], BYTE 1
	neg eax
di99:   push eax
NEXT

; -------------------------------------------------------------------------------------
DefCode "#", 1, 0, DOTlb                ; ( u1--u2 )
        pop eax
        xor edx, edx
        idiv DWORD [v_BASE]
        push eax
        add edx, '0'
        cmp edx, '9'
        jle lb1
        add edx, 7
lb1:    mov eax, [dotPtr]
        dec eax
        mov [eax], BYTE dl
        mov [dotPtr], eax
        inc BYTE [dotLen]
        NEXT

; -------------------------------------------------------------------------------------
DefCode "#>", 2, 0, DOTdone
        pop eax                         ; Discard garbage 0 left on stack from #S
        cmp [isNeg], BYTE 1             ; Prepend the '-' if necessary
        jne dd99
        inc BYTE [dotLen]
        mov eax, [dotPtr]
        dec eax
        mov [eax], BYTE '-'
        mov [dotPtr], eax
dd99:   movzx eax, BYTE [dotLen]
        push [dotPtr]
        push eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "HERE",4,0,HERE
        push DWORD [v_HERE]
        NEXT

; -------------------------------------------------------------------------------------
DefCode "LAST",4,0,LAST
        push DWORD [v_LAST]
        NEXT

; -------------------------------------------------------------------------------------
DefCode ">R",2,0,ToR
        pop eax
        rPUSH eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "R@",2,0,RAt
        mov eax, DWORD RTOS
        push eax
        NEXT

; -------------------------------------------------------------------------------------
DefCode "R>",2,0,RFrom
        rPOP eax
        push eax
        NEXT

; -------------------------------------------------------------------------------------
; toNum: Implementation for the Forth CORE word >NUMBER
;       Params: EDX: num, EDI: string, ECX:-len 
;       Return: EDX: num, EDI: string, ECX:-len (0 means all converted)
toNum:  xor eax, eax
        mov ebx, [v_BASE]
tn01:   test ecx, ecx           ; End of string?
        jz tnX
        mov al, [edi]
        call digitQ
        test ah, ah             ; AH == 0 if it is a number
        jnz tnX
        imul edx, ebx
        add edx, eax
        inc edi
        dec ecx
        jmp tn01
tnX :   ret

; -------------------------------------------------------------------------------------
; >Number: Standard Forth def - ( num addr len -- num2 addr2 len2 )
DefCode ">NUMBER",7,0,TONUM
        pop ecx         ; starting len
        pop edi         ; starting addr
        pop edx         ; starting num
        call toNum
        push edx        ; ending num
        push edi        ; ending addr
        push ecx        ; ending len
        NEXT

; -------------------------------------------------------------------------------------
DefWord "EVAL",4,0,EVAL
        dd OK, TIB, LIT, 128, ACCEPT, DECTOS, zBRANCH, evEOL
        dd LIT, tib, LIT, curIn, fSTORE
evLP:   dd xtWORD, CFETCH, zBRANCH, evEOL
        dd LIT, 0, LIT, curWord, COUNT
        dd TONUM, nzBRANCH, evWD, DROP
        dd STATE, FETCH, zBRANCH, evLP
        dd LIT, LIT, COMMA, COMMA
        dd BRANCH, evLP
evWD:   dd DROP2, LIT, curWord
        dd FIND, qDUP, zBRANCH, evERR
        dd DECTOS, zBRANCH, evEXEC
        dd STATE, FETCH, zBRANCH, evEXEC
        dd COMMA, BRANCH, evLP
evEXEC: dd EXECUTE, BRANCH, evLP
evERR:  dd LIT, '-', EMIT, COUNT, TYPE
        dd LIT, '?', EMIT, QUIT
evEOL:  dd EXIT

; -------------------------------------------------------------------------------------
; -------------------------------------------------------------------------------------
; -------------------------------------------------------------------------------------
DefWord "QUIT",4,0,QUIT
        dd zRSP
qLOOP:  dd EVAL, BRANCH, qLOOP

; -------------------------------------------------------------------------------------
DefWord "CR",2,0,CR
        dd LIT, 13, EMIT, LIT, 10, EMIT, EXIT

; -------------------------------------------------------------------------------------
DefWord "BL",2,0,fBL
        dd LIT, 32, EXIT

; -------------------------------------------------------------------------------------
DefWord "SPACE",5,0,SPACE
        dd fBL, EMIT, EXIT

; -------------------------------------------------------------------------------------
DefWord "OK",2,0,OK
        dd SPACE, LIT, 'O', EMIT, LIT, 'K', EMIT
        dd CR, EXIT

; -------------------------------------------------------------------------------------
DefWord "TIB",3,0,TIB
        dd LIT, tib, EXIT

; -------------------------------------------------------------------------------------
DefWord ">IN",3,0,TOIN
        dd LIT, curIn, EXIT

; -------------------------------------------------------------------------------------
DefWord ":",1,0,xtDEFINE
        dd CREATE, LIT, DOCOL, COMMA
        dd STATE1, EXIT

; -------------------------------------------------------------------------------------
DefWord ";",1,IMMEDIATE,ENDWORD
        dd LIT, EXIT, COMMA
        dd STATE0, EXIT

; -------------------------------------------------------------------------------------
DefWord "'",1,0,TICK
        dd xtWORD, FIND, EXIT

; -------------------------------------------------------------------------------------
DefWord "[",1,IMMEDIATE,STATE0
        dd LIT, 0, STATE, fSTORE, EXIT

; -------------------------------------------------------------------------------------
DefWord "]",1,0,STATE1
        dd LIT, 1, STATE, fSTORE, EXIT

; -------------------------------------------------------------------------------------
DefWord "#S", 2, 0, DOTlbS              ; ( u--0)
lbS1:   dd DOTlb, DUP1, nzBRANCH, lbS1
        dd EXIT

; -------------------------------------------------------------------------------------
DefWord "(.)", 3, 0, DOTn
        dd DOTinit, DOTlbS, DOTdone, TYPE, EXIT

; -------------------------------------------------------------------------------------
DefWord ".", 1, 0, DOT
        dd DOTn, SPACE, EXIT

; -------------------------------------------------------------------------------------
DefWord "BENCH",5,0,BENCH
        dd TIMER, SWAP, LIT, 0, xtDO, xtLOOP, TIMER, SWAP, xtSUB, DOT
        dd EXIT

; -------------------------------------------------------------------------------------
; Some variables ...
; -------------------------------------------------------------------------------------
DefVar "STATE",5,0,STATE
DefVar "BASE",4,0,BASE
DefVar "USER",4,0,MEM

; -------------------------------------------------------------------------------------
; -------------------------------------------------------------------------------------
; -------------------------------------------------------------------------------------
match =WINDOWS, FOR_OS { section '.bdata' readable writable }
match =LINUX,   FOR_OS { segment readable writable }

; -------------------------------------------------------------------------------------
ver db 'f3 v0.1', 13, 10, 0

InitialESP dd 0
InitialEBP dd 0

rStack   dd 64 dup (0)          ; The return stack
lStack   dd 64 dup (0)          ; The loop stack
lSP      dd lStack              ; The loop stack-pointer

regs     dd  100 dup (0)        ; My pseudo-registers
rbase    dd    0

curIn    dd    0                ; curIn - pointer to current char in TIB
tib      db  128 dup (0)        ; The Text Input Buffer
curWord  db   32 dup (0)        ; The current word
buf4     db    4 dup (0)        ; A buffer for EMIT (LINUX)

isNeg    db  0
dotLen   db  0
dotPtr   dd  0
dotBuf   db 64 dup (0)

v_HERE:  dd xHERE
v_LAST:  dd LastTag
v_STATE: dd 0
v_BASE:  dd 10

xHERE:
v_MEM:   rb MEM_SZ  
MEM_END:
