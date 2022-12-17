.386
.model flat,stdcall
.stack 4096
ExitProcess proto,dwExitCode:dword

.data
include \library\osama.inc	

.code
main PROC
	

INVOKE ExitProcess,0
main ENDP
END main

