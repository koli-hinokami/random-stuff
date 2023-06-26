macro pascalcall proc,[arg]
{
    common
	if ~ eq	arg
    forward
	push arg
    common
	end if
	call proc
}
;Fastcall macro for interrupts
;mov's operand into registers in order they are specified and interrupt's
macro	ficall	intn,	[reg,	opr]	{
	forward
	mov	reg,	opr
	common
	int	intn
}
;Fastcall macro for functions
;basically same but call instead of interrupt
macro	fcall	fnptr,	[reg,	opr]	{
	forward
	mov	reg,	opr
	common
	call	fnptr
}
;ASCIIZ string definition
;also does size calculation
;stolen from somewhere in KolibriOS macro-libraries
macro szz name,[data] {
 common
  if used name
   forward
    name db data
   common
   db 0
   .size = $-name
  end if
}
;ASCIIZ String array
macro	stringarray	[str]	{
	forward
		local	lbl
		dw	lbl
	common	
		dw	0
	forward
		lbl:	db str,0
}
macro	multiline	[opr]	{
	forward
	opr
}
macro	cli	op1	{
	cli
	op1
}
; mov between segments extension
macro	mov	op1,	op2 {
	if op1 in <ds,es,fs,gs,ss> & op2 in <cs,ds,es,fs,gs,ss>
		push  op2
		pop   op1
	else
		mov   op1,op2
	end if
}
; mov with three operands extension
macro	mov	op1,	op2,	op3 {
	if op3 eq
		mov   op1,op2
	else
		mov   op1,op3
		mov   op2,op1
	end if
}
; enter macro
macro	enter	op1	{
	push	bp
	mov	bp,	sp
	if	op1 eq
	else
		sub	sp,	op1
	end if
}
; leave macro
macro	leave	{
	mov	sp,	bp
	pop	bp
}
