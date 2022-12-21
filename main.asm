.386
.model flat,stdcall
.stack 4096
include Irvine32.inc
ExitProcess proto,dwExitCode:dword

.data
multiplySymbol BYTE 42 ;2Ah
plusSymbol BYTE 43 ;2Bh
minusSymbol BYTE 45 ;2Dh
divideSymbol BYTE 47 ;2Fh

inputString BYTE 200 dup(?)
sizeOfInputString DWORD ?
inputStringPointer BYTE 0

stack1 WORD 200 dup(0)
stack1pointer BYTE ?
stack1size BYTE 0

stack2 BYTE 200 dup(0)
stack2Pointer BYTE 0
stack2size BYTE 0

;postfexString BYTE 200 dup(?)
postfexString BYTE "112*+32*3/-1+"
postfexStringPointer BYTE 0
alPrecedenceLevel BYTE 0
stack1topPrecedenceLevel BYTE 0

currentOperation BYTE 0

zero BYTE 0
temp WORD ?
result BYTE 0


.code
;-----------------------------------main
main PROC
lea edx, postfexString
call WriteString
call Crlf
;call readInputString
;call infexToPostfex
call evaluatePostfex
movzx eax, result
call WriteDec

INVOKE ExitProcess,0
main ENDP
;-----------------------------------

;-----------------------------------ReadInputString
readInputString PROC
mov eax, 0
lea edx, inputString
mov ecx, 201
call ReadString
mov sizeOfInputString, eax
ret
readInputString ENDP
;-----------------------------------

;-----------------------------------infexToPostfex
infexToPostfex PROC
mov ecx, sizeOfInputString
L1:
push eax
push ebx
movzx ebx, inputStringPointer
mov al, inputString[ebx]
pop ebx
pop eax
call isOperator
jz operator
call processOperand1
jmp theEnd
operator:
call processOperator1
inc inputStringPointer
LOOP L1
theEnd:
ret
infexToPostfex ENDP
;-----------------------------------

;-----------------------------------isOperator
isOperator PROC
cmp al, 42 ;if true -> zero flag is set
je operator
cmp al, 43 ;if true -> zero flag is set
je operator 
cmp al, 45 ;if true -> zero flag is set
je operator
cmp al, 47 ;if true -> zero flag is set
je operator
jmp notAnOperator
operator:
ret
notAnOperator:
ret
isOperator ENDP
;-----------------------------------
 
;-----------------------------------processOperator1
processOperator1 PROC
;while stack not empty {if isHigherPrecedence then pushToStack else {pop, add popped to output}}
startWhile:
call isStack1Empty
je emptyStack
;stack is not empty
call isHigherPrecedence1
je higherPrecedence
;equal or lower precedence:
call popStack1
call addToPostfexString
jmp continueWhile

higherPrecedence:
call pushToStack1
jmp theEnd

continueWhile:
jmp startWhile
emptyStack:
call pushToStack1
theEnd:


push ecx
movzx ecx, stack1size
L1:

call addToPostfexString
LOOP L1
pop ecx
ret
processOperator1 ENDP
;-----------------------------------

;-----------------------------------isStack1Empty
isStack1Empty PROC
cmp stack1size, 0
ret
isStack1Empty ENDP
;-----------------------------------

;-----------------------------------pushToStack1
pushToStack1 PROC
push ebx
movzx ebx, stack1size
lea edi, stack1[ebx*2]
pop ebx

cmp al, 2bh
je plusOrMinus
cmp al, 2dh
je plusOrMinus
cmp al, 2ah
je multiplyOrDivide
cmp al, 2fh
je multiplyOrDivide
jmp continue


plusOrMinus:
mov WORD PTR [edi], 1
mov BYTE PTR [edi+1], al
Inc stack1size
jmp continue
multiplyOrDivide:
mov WORD PTR [edi], 2
mov BYTE PTR [edi+1], al
Inc stack1size
jmp continue

continue:
ret
pushToStack1 ENDP
;-----------------------------------

;-----------------------------------isHigherPresedence1
;does whatever is in the al right now have a higher precedence than the top element of the stack?
;if yes -> cmp zero, 0 else -> cmp zero, 1
isHigherPrecedence1 PROC
;if al=+ or - then alPrecedenceLevel=1 else alPrecedenceLevel=2
;if stack1top= + or - then stack1topPrecedenceLevel=1 else stack1topPrecedenceLevel=2

cmp al, 2bh
je alPlusOrMinusSign
cmp al, 2dh
je alPlusOrMinusSign
mov alPrecedenceLevel, 2
jmp continue
alPlusOrMinusSign:
mov alPrecedenceLevel, 1
continue:

push ebx
movzx ebx, stack1size
lea edi, stack1[ebx*2]
pop ebx

cmp BYTE PTR [edi+1], 2bh
je sPlusOrMinusSign
cmp BYTE PTR [edi+1], 2dh
je sPlusOrMinusSign
mov stack1topPrecedenceLevel, 2
jmp cmpPrecedence
sPlusOrMinusSign:
mov stack1topPrecedenceLevel, 1

cmpPrecedence:
;al > stacktop -> cmp zero, 0
;al <= stacktop -> cmp zero, 1
cmp al, BYTE PTR [edi+1]
ja alHigher
cmp zero, 1
jmp theEnd
alHigher:
cmp zero, 0
theEnd:
ret
isHigherPrecedence1 ENDP
;-----------------------------------



;-----------------------------------popStack1
popStack1 PROC
push ebx
movzx ebx, stack1size
lea edi, stack1[ebx*2]
pop ebx

push edx
mov dx, WORD PTR [edi]
mov temp, dx
pop edx
dec stack1size
ret
popStack1 ENDP
;-----------------------------------

;-----------------------------------processOperand1
processOperand1 PROC
push ebx
movzx ebx, postfexStringPointer
mov postfexString[ebx], al
pop ebx
inc postfexStringPointer
ret
processOperand1 ENDP
;-----------------------------------

;-----------------------------------addToPostfexString 
addToPostfexString PROC
;mov from temp to postfex string and inc the pointer
push eax
push ebx
mov ax, temp
movzx ebx, postfexStringPointer
mov postfexString[ebx], al
pop ebx
pop eax
inc postfexStringPointer
ret
addToPostfexString ENDP
;-----------------------------------

;-----------------------------------evaluatePostfex
;returns the result into the variable called result
evaluatePostfex PROC
mov ecx, lengthof postfexString
L1:
movzx ebp, postfexStringPointer
mov al, postfexString[ebp]
call isOperator ;if operator -> zero flag is set
jz operator
;operand
sub al, 30h ;because "0" is 30h (convert string to int)
call pushAlToStack2
inc postfexStringPointer
jmp continue
operator:
;pop last two into b, d, check the operation, do that operation, push the result to stack2
dec stack2Pointer
movzx ebp, stack2Pointer
mov bl, stack2[ebp]
dec stack2Pointer
movzx ebp, stack2Pointer
mov dl, stack2[ebp]
call performOperation
call pushBlToStack2 ;push result (from bl) into stack2
inc postfexStringPointer
continue:
LOOP L1

;at the end, only the result will be in stack2
dec stack2Pointer
movzx ebp, stack2Pointer
mov bl, stack2[ebp]
mov result, bl
ret
evaluatePostfex ENDP
;-----------------------------------

;-----------------------------------performOperation
performOperation PROC
; bl = bl al dl
cmp al, 2ah
je multiplication
cmp al, 2bh
je addition
cmp al, 2dh
je subtraction
cmp al, 2fh
je division

multiplication:
push eax
mov al, bl
mul dl ;result is in ax
mov bl, al ;take the lower part of result (because upper part won't be used)
pop eax
jmp theEnd
addition:
add bl, dl
jmp theEnd
subtraction:
sub dl, bl
mov bl, dl
jmp theEnd
division:
push eax
movzx ax, dl
div bl ;div -> ax / bl = al (%remainder = ah)
mov bl, al
pop eax

theEnd:
ret
performOperation ENDP
;-----------------------------------

;-----------------------------------pushAlToStack2
;push al to stack2
pushAlToStack2 PROC
movzx ebp, stack2Pointer
mov stack2[ebp], al
inc stack2Pointer
ret
pushAlToStack2 ENDP
;-----------------------------------

;-----------------------------------pushBlToStack2
;push bl to stack2
pushBlToStack2 PROC
movzx ebp, stack2Pointer
mov stack2[ebp], bl
inc stack2Pointer
ret
pushBlToStack2 ENDP
;-----------------------------------


;-----------------------------------isHigherPrecedence
isHigherPrecedence PROC
; * : 42 (2Ah)
; + : 43h (2Bh)
; - : 44h (2Dh)
; / : 45h (2Fh)

push ebx
movzx ebx, stack1size
lea edi, stack1[ebx*2]
pop ebx
cmp al, BYTE PTR [edi]
ret
isHigherPrecedence ENDP
;-----------------------------------

END main