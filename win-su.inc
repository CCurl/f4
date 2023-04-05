; ------------------------------------------------------------------------------
format PE console 

section '.idata' data readable import

include 'win32ax.inc'
library kernel32, 'kernel32.dll', msvcrt, 'msvcrt.dll', conio, 'conio.dll'
import msvcrt, putchar, 'putchar', _getch, '_getch', _kbhit, '_kbhit', gets, 'gets'
import kernel32, GetTickCount, 'GetTickCount'

; section '.bdata' data readable writable
