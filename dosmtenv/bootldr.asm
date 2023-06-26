	include "if.inc"
	include	"proc16.inc"
	include	"struct.inc"
	include "x86.inc"
	;include "macros.inc"
	include "custommacro.inc"
	format MZ
	entry	bootstrapperflat:bootstrapperentry

;-------------------------------------------------------
;--------------------- Microkernel ---------------------
;-------------------------------------------------------
	segment microkernelflat
	

szKeInitString:	db 13,10,13,10,'MTENV Microkernel',13,10,13,10,0

KiInitializeIntVectors:
	push	bx
	intDbgMultiplex	= 0A1h
	fcall	RtlSetNearIntVec, cl,0xA1, dx, DbgMultiplex
	intKeMultiplex	= 9Fh
	fcall	RtlSetNearIntVec, cl, 9Fh, dx, KeMultiplex
	intExSyscall	= 90h
	fcall	RtlSetNearIntVec, cl, 90h, dx, ExSyscall
	intKiQuantumEnd	= 92h
	fcall	RtlSetNearIntVec, cl, 92h, dx, KiQuantumEnd
	intKiQuantumBegin = 93h
	fcall	RtlSetNearIntVec, cl, 93h, dx, KiQuantumBegin
	intKiLoadTss	= 94h
	fcall	RtlSetNearIntVec, cl, 94h, dx, KiLoadTss
	intKiStoreTss	= 95h
	fcall	RtlSetNearIntVec, cl, 95h, dx, KiStoreTss
	intKeYield	= 96h
	fcall	RtlSetNearIntVec, cl, 96h, dx, KeYield
	intKeSwitchStack = 9Ch
	fcall	RtlSetNearIntVec, cl, 9Ch, dx, KeSwitchStack
	;intKiNonmaskable = 2
	;fcall	RtlSetNearIntVec, cl, 2, dx, KiNonmaskable
	intKiTrap = 3
	fcall	RtlSetNearIntVec, cl, 3, dx, KiTrap
	intGdiMultiplex	= 0xA2		
	fcall	RtlSetNearIntVec, cl, intGdiMultiplex,	dx, KiDummyInterrupt
	intWsMultiplex	= 0xA3
	fcall	RtlSetNearIntVec, cl, intWsMultiplex,	dx, KiDummyInterrupt
	intVdMultiplex	= 0xA4
	fcall	RtlSetNearIntVec, cl, intVdMultiplex,	dx, KiDummyInterrupt
	intExMultiplex	= 0xA0
	fcall	RtlSetFarIntVec,  cl, intExMultiplex, dx,ExMultiplex, ax,executiveflat
	pop	bx
	ret
RtlSetNearIntVec: ;void (dx near void(*)(), cl unsigned char intnumber)
	push	ds
	push	cs
	pop	ds
	mov	al,	cl
	mov	ah,	25h
	int	21h
	pop	ds
	ret
RtlSetFarIntVec: ;void (ax:dx far void(*)(), cl unsigned char intnumber)
	push	bx	ds
	mov	bx,	ds,	0
	mov	bl,	cl
	mov	bh,	0
	add	bx,	bx
	add	bx,	bx
	pushf
	cli
	mov	[intvectable+bx],	dx
	mov	[intvectable+bx+2],	ax
	popf
	pop	ds	bx
	ret
KiDummyInterrupt:
	iret
KiNonmaskable:
	int	intKiQuantumEnd
	
	int	intKiQuantumBegin
	iret
KiTrap:
	cli
	push	ax bx cx dx si di bp ds es
	push	bp
	mov	bp,	sp
	;sub	sp,	16	
	virtual at  bp+0	;frame:
	.fbp	dw ?		;bp+0 	sp+0 	^∙frame bp
	.res	dw ?		;bp+2	sp+2	|∙es
	.rds	dw ?		;bp+4	sp+4	|∙ds
	.rbp	dw ?		;bp+6	sp+6	|∙bp
	.rdi	dw ?		;bp+8	sp+8	|∙di
	.rsi	dw ?		;bp+10	sp+10	|∙si
	.rdx	dw ?		;bp+12	sp+12	|∙dx
	.rcx	dw ?		;bp+14	sp+14	|∙cx
	.rbx	dw ?		;bp+16	sp+16	|∙bx
	.rax	dw ?		;bp+18	sp+18	|∙ax
	.rip	dw ?		;bp+20	sp+20	|∙ip
	.rcs	dw ?		;bp+22	sp+22	|∙cs
	.rfl	dw ?		;bp+24	sp+24	|∙flags
	.prevlocals:	dw ?	;bp+26	sp+26	|∙prev.locals
	end virtual				
	mov	ax,	cs
	mov	ds,	ax
	mov	es,	ax
.main:	fcall	DbgPuts,  dx,.str1
	fcall	DbgPrintUint16Hex, dx,[.rcs]
	fcall	DbgPutch, dl,':'
	fcall	DbgPrintUint16Hex, dx,[.rip]
	fcall	DbgPutch, dl,13
	fcall	DbgPutch, dl,10
	;AX-BP
	mov	si,	.regs
	mov	di,	.rax-bp
	push	bx
	mov	bx,	7
@@:	lodsw
	fcall	DbgPuts, dx,ax
	fcall	DbgPrintUint16Hex, dx,[bp+di]
	add	di,	2
	dec	bx
	jnz	@b
	;SP
	lodsw
	fcall	DbgPuts, dx,ax
	lea	dx,	[.prevlocals]
	call	DbgPrintUint16Hex
	;New line
	fcall	DbgPutch, dl,13
	fcall	DbgPutch, dl,10
	;CS-SS
	lodsw
	fcall	DbgPuts, dx,ax
	fcall	DbgPrintUint16Hex, dx,[.rcs]
	lodsw
	fcall	DbgPuts, dx,ax
	fcall	DbgPrintUint16Hex, dx,[.rds]
	lodsw
	fcall	DbgPuts, dx,ax
	fcall	DbgPrintUint16Hex, dx,[.res]
	lodsw
	fcall	DbgPuts, dx,ax
	fcall	DbgPrintUint16Hex, dx,   ss
	;IP&FL
	lodsw
	fcall	DbgPuts, dx,ax
	fcall	DbgPrintUint16Hex, dx,[.rip]
	lodsw
	fcall	DbgPuts, dx,ax
	fcall	DbgPrintUint16Hex, dx,[.rfl]
	;New line
	fcall	DbgPutch, dl,13
	fcall	DbgPutch, dl,10
	;CS Dump
	fcall	DbgPuts, dx,.str2
	mov	di,	[.rip]
	lea	si,	[di-16]
	push	es
	mov	ax,	es,	[.rcs]
	call	.printdumpline
	call	.printdumpline
	call	.printdumpline
	pop	es
	;New line
	fcall	DbgPutch, dl,13
	fcall	DbgPutch, dl,10
	;Stacktrace
	fcall	DbgPuts, dx,.str3
	mov	si,	[.rbp]
	call	.dumpsingleframe
	call	.dumpsingleframe
	call	.dumpsingleframe
	call	.dumpsingleframe
	;New line
	fcall	DbgPutch, dl,13
	fcall	DbgPutch, dl,10
	call	KiDumpTaskList
.finalize:
	;mov	ax,	[bp+28]
	;or	ax,	0x100
	;mov	[bp+28],	ax
.ret:	mov	sp,	bp
	pop	bp
	pop	es ds bp di si dx cx bx ax
	iret
.str1:	db	'      <!Trap occured!> at ',0
.str2:  db	'Code executed:',0
.str3:	db	'Stack trace:',13,10,0
.printdumpline:
	;SI	address
	;DI	highlight address
	fcall	DbgPutch, dl,13
	fcall	DbgPutch, dl,10
	fcall	DbgPrintUint16Hex, dx,es
	fcall	DbgPutch, dl,':'
	fcall	DbgPrintUint16Hex, dx,si
	.if	si=di
	fcall	DbgPutch, dl,'>'
.else	fcall	DbgPutch, dl,' '
.endif	push	bx
	mov	bx,	16
@@:	es	lodsb
	fcall DbgPrintUint8HexPrefixless, dl,al
	fcall	DbgPutch, dl,' '
	dec	bx
	jnz	@b
	pop	bx
	sub	si,	16
	push	bx
	mov	bx,	16
@@:	es lodsb
	fcall	DbgPutchRaw, dl,al
	dec	bx
	jnz	@b
	pop	bx
	ret
.dumpsingleframe:
	;SI	stack frame to dump
	fcall	DbgPutch, dl,'S'
	fcall	DbgPutch, dl,'S'
	fcall	DbgPutch, dl,':'
	fcall	DbgPrintUint16Hex, dx,[ss:si]	;Frame
	fcall	DbgPutch, dl,' '
	fcall	DbgPrintUint16Hex, dx,[ss:si+2]	;Address of subr. to who frame is bound
	fcall	DbgPutch, dl,' '
	fcall	DbgPrintUint16Hex, dx,[ss:si+4]	;Same but for far call
	fcall	DbgPutch, dl,':'
	fcall	DbgPrintUint16Hex, dx,[ss:si+2]	
	mov	si,	[ss:si]	;Unwind a frame
				;word [bp] is previous bp
	fcall	DbgPutch, dl,13
	fcall	DbgPutch, dl,10
	ret
.regs:	stringarray	' AX:',' BX:',' CX:',' DX:',' SI:',' DI:',\
			' BP:',' SP:',' CS:',' DS:',' ES:',' SS:',\
			' IP:',' FL:'
KiDumpTaskList:
	push	bx	si
.main:	fcall	DbgPuts, dx,.str1
	mov	si,	processtable.state
	mov	dx,	0
	mov	ch,	0
.loop:	mov	cl,	7
	mov	bx,	dx
	shl	bx,	cl
	cmp	byte [cs:bx+si],	kProcess.state.free
	jne	.found
	inc	dl
	jz	.ret
	jmp	.loop
.found:	push	dx
	mov	bl,	dl	;form base pointer
	mov	bh,	0
	mov	cl,	7
	shl	bx,	cl
	fcall	DbgPrintUint16Hex, dx,[cs:bx+processtable.tid]
	fcall	DbgPutch, dl,':'
	lea	si,	[cs:bx+processtable.name]
	mov	cx,	11
@@:	cs	lodsb
	push	cx
	fcall	DbgPutch, dl,al
	pop	cx
	loop	@b	
	fcall	DbgPutch, dl,'@'
	fcall	DbgPrintUint16Hex, dx,[cs:bx+processtable.rcs]
	fcall	DbgPutch, dl,':'
	fcall	DbgPrintUint16Hex, dx,[cs:bx+processtable.rip]
	fcall	DbgPutch, dl,' '
	fcall	DbgPrintUint16Hex, dx,[cs:bx+processtable.rss]
	fcall	DbgPutch, dl,':'
	fcall	DbgPrintUint16Hex, dx,[cs:bx+processtable.rsp]
	fcall	DbgPutch, dl,' '
	fcall	DbgPrintUint16Hex, dx,[cs:bx+processtable.rbp]
	fcall	DbgPutch, dl,' '
	fcall	DbgPrintUint8HexPrefixless, dx,[cs:bx+processtable.state]
	fcall	DbgPutch, dl,13
	fcall	DbgPutch, dl,10
	pop	dx
	inc	dl
	jz	.ret
	jmp	.loop
.ret:	pop	si	bx
	ret
.str1:	db 'Task list:',13,10,0
KeInitialization:
	;INT 91 First call: Initialization of kernel
	mov	cl,	0x91
	mov	dx,	KeInitialization2
	call	RtlSetNearIntVec
	mov	dx,	szKeInitString
	mov	es,	cs
	call	DbgPuts
	call	KiInitializeIntVectors
	iret
KeInitialization2:
	;INT 91 Second call: Second initialization of kernel and 
	;initialization of modules
	mov	cl,	0x91
	mov	dx,	KeInitialization3
	call	RtlSetNearIntVec
	ficall	intDbgMultiplex,	al,1
	ficall	intVdMultiplex,		al,1
	ficall	intGdiMultiplex,	al,1
	ficall	intWsMultiplex,		al,1
	call	KiInitMultitasking
	iret
KeInitialization3:
	;INT 91 Third call: Bootstrapping of kernel and flying away from DOS
	call	KiRunMultitasking
	iret
macro	ltr	op1	{
	mov	dx,	op1
	int	intKiLoadTss
}
KiLoadTss:
	mov	[cs:roottss.tssbase],	dx
	push	es	
	mov	es,	cs	;ES:DI <- CS:roottss
	mov	di,	roottss
	mov	si,	dx	;CS:SI <- dx
	mov	cx,	roottss.size
	cli
	cld
	rep	cs	movsb	
	pop	es
	iret
macro	str	op1	{
	mov	dx,	op1
	int	intKiStoreTss
}
KiStoreTss:
	push	es	bx
	;ES:DI <- CS:dx
	;CS:SI <- roottss
	mov	si,	roottss
	mov	di,	dx
	mov	es,	cs
	mov	cx,	roottss.size
	cli
	cld
	rep	cs	movsb	
	pop	bx	es
	iret
DbgPuts: ;void fastcall (es:dx far char* string)
	push	si
	mov	si,	dx
.loop:	lods	byte [es:si]
	test	al,	al
	jz	.ret
	mov	dl,	al
	call	DbgPutch
	jmp	.loop
.ret:	pop	si
	ret
DbgPutch: ;void fastcall (dl char)
	mov	cl,	dl
	mov	dx,	0x3F8+5
@@:	in	al,	dx
	test	al,	0x20
	jz	@b
	mov	dx,	0x3F8
	mov	al,	cl
	out	dx,	al
	ret
DbgPrintUint16Hex: ;void fastcall (dx uint16_t)
	push	bx	si
	mov	si,	dx
	fcall	DbgPutch, dl,'0'	
	fcall	DbgPutch, dl,'x'	
	mov	cx,	4
@@:	mov	bx,	0
	shl	si,	1
	rcl	bx,	1
	shl	si,	1
	rcl	bx,	1
	shl	si,	1
	rcl	bx,	1
	shl	si,	1
	rcl	bx,	1
	mov	dl,	[cs:bx+.tbl]
	push	cx
	call	DbgPutch
	pop	cx
	loop	@b
	pop	si	bx
	ret
.tbl:	db	'0123456789ABCDEF',0
DbgPrintUint8HexPrefixless: ;void fastcall (dx uint16_t)
	push	bx	si
	mov	si,	dx
	mov	cl,	8
	shl	si,	cl
	mov	cx,	2
@@:	mov	bx,	0
	shl	si,	1
	rcl	bx,	1
	shl	si,	1
	rcl	bx,	1
	shl	si,	1
	rcl	bx,	1
	shl	si,	1
	rcl	bx,	1
	mov	dl,	[cs:bx+.tbl]
	push	cx
	call	DbgPutch
	pop	cx
	loop	@b
	pop	si	bx
	ret
.tbl:	db	'0123456789ABCDEF',0
DbgPutchRaw:
	cmp	dl,	32
	jae	.print
	mov	dl,	'.'
.print: jmp	DbgPutch
	ret
RtlGetIntVec: ;fastcall (void(interrupt far*)())()(dl intnumber)
	push	bx	es
	mov	ah,	0x35
	mov	al,	dl
	int	21h
	mov	dx,	es
	mov	ax,	bx
	pop	es	bx
	ret
KiHookedInt8:
	cli
	int	intKiQuantumEnd
	str	[cs:roottss.tssbase]
	call	KiSchedulerFindThread
	ltr	ax
	pushf
	call	dword	[cs:.prevint8]
	mov	al,	0x20
	out	0x20,	al
	int	intKiQuantumBegin
	iret
.prevint8:	dd 0
KeYield:
	cli
	int	intKiQuantumEnd
	str	[cs:roottss.tssbase]
	call	KiSchedulerFindThread
	ltr	ax
	int	intKiQuantumBegin
	iret
KiHookInt8:
	mov	dl,	8
	call	RtlGetIntVec
	mov	word [cs:KiHookedInt8.prevint8], ax
	mov	word [cs:KiHookedInt8.prevint8+2], dx
	mov	cl,	8
	mov	dx,	KiHookedInt8
	call	RtlSetNearIntVec
	ret
KiInitMultitasking:
	call	KiInitProcessTable
	ret
KiRunMultitasking:
	cli
	call	KiHookInt8
	ltr	processtable+kProcess.size*1
	sti
	;jmp	$
	;mov	ax,	eflags.if
	;push	ax
	;push	cs
	;push	.cont
	;iret
.cont:
	ret
KiInitProcessTable:
	push	bx
	;init all slots state to free
	mov	cx,	256
	mov	bx,	processtable.state
@@:	mov	byte [cs:bx], kProcess.state.free
	add	bx,	kProcess.size
	loop	@b
	;Initialize "System" process
	mov	[cs:kProcess.size*0+processtable.state], kProcess.state.runnable
	mov	[cs:kProcess.size*0+processtable.kss],	microkernelkstack
	mov	[cs:kProcess.size*0+processtable.ksp],	microkernelksp
	mov	[cs:kProcess.size*0+processtable.rss],	microkernelstack
	mov	[cs:kProcess.size*0+processtable.rsp],	microkernelsp
	mov	[cs:kProcess.size*0+processtable.rcs],	microkernelflat
	mov	[cs:kProcess.size*0+processtable.rip],	KiSystemthreadEntry
	mov	[cs:kProcess.size*0+processtable.rfl],	eflags.if
	mov	word [cs:kProcess.size*0+processtable.name+0], 'OS'
	mov	word [cs:kProcess.size*0+processtable.name+2], '/I'
	mov	word [cs:kProcess.size*0+processtable.name+4], 'DL'
	mov	word [cs:kProcess.size*0+processtable.name+6], 'E '
	mov	word [cs:kProcess.size*0+processtable.name+8], '  '
	mov	byte [cs:kProcess.size*0+processtable.name+9], ' '
	;Initialize shell process
	mov	[cs:kProcess.size*1+processtable.state], kProcess.state.runnable
	mov	[cs:kProcess.size*1+processtable.kss],	microkernelkstack2
	mov	[cs:kProcess.size*1+processtable.ksp],	microkernelksp2
	mov	word [cs:kProcess.size*1+processtable.name+0], 'DO'
	mov	word [cs:kProcess.size*1+processtable.name+2], 'S '
	mov	word [cs:kProcess.size*1+processtable.name+4], 'Sh'
	mov	word [cs:kProcess.size*1+processtable.name+6], 'el'
	mov	word [cs:kProcess.size*1+processtable.name+8], 'l '
	mov	byte [cs:kProcess.size*1+processtable.name+9], ' '
	; v These four are written by scheduler in KiQuantumEnd()
	; from DOS Enviroment thread
	mov	[cs:kProcess.size*1+processtable.rss],	9999
	mov	[cs:kProcess.size*1+processtable.rsp],	9999
	mov	[cs:kProcess.size*1+processtable.rcs],	9999
	mov	[cs:kProcess.size*1+processtable.rip],	9999
	mov	[cs:kProcess.size*1+processtable.rfl],	eflags.if
	;mov	[cs:processtable.],	
	int3
	pop	bx
	ret
KiSchedulerReset:
	iret
KiSchedulerFindThread: ;ax near kProcess* thread (void)
	push	bx	si
	mov	si,	processtable.state
	mov	dl,	[cs:.position]
	mov	dh,	0
	mov	ch,	0
@@:	mov	cl,	7
	mov	bx,	dx
	shl	bx,	cl
	cmp	byte [cs:bx+si],	kProcess.state.runnable
	je	@f
	inc	dl
	;dec	ch
	jmp	@b	
	;ud2 
@@:	inc	dl
	mov	[cs:.position],	dl
	dec	dl
	mov	al,	dl
	mov	ah,	0
	mov	cl,	7
	shl	ax,	cl
	add	ax,	processtable
	pop	si	bx
;	mov	ax,	processtable
	ret
.position:	db 0 ; static int
KiSystemthreadEntry:
	mov	es,	cs
	fcall	DbgPuts,dx,.str1
	int	intKeYield
	loop	$
	jmp	KiSystemthreadEntry
	jmp	KiSystemthreadEntry
.str1:	db	' Multitasking running!',13,10,0
KiSecondsystemthreadEntry:
	loop	$
	jmp	KiSecondsystemthreadEntry
	jmp	KiSecondsystemthreadEntry

KiQuantumEnd:
	cli
	;
	mov		[cs:roottss.rax],	ax
	mov		[cs:roottss.rbx],	bx
	mov		[cs:roottss.rcx],	cx
	mov		[cs:roottss.rdx],	dx
	mov		[cs:roottss.rsi],	si
	mov		[cs:roottss.rdi],	di
	mov		[cs:roottss.rbp],	bp
	mov	ax,	[cs:roottss.rds],	ds
	mov	ax,	[cs:roottss.res],	es
	mov	ax,	[cs:roottss.rss],	ss
	mov	bp,	sp
	add	sp,	12
	mov	dx,	[bp+0]
	mov	si,	[bp+2]
	mov	di,	[bp+4]
	or	di,	eflags.if
	mov		[cs:roottss.rsp],	sp
	mov	ax,	[cs:roottss.rip],	[bp+6]
	mov	ax,	[cs:roottss.rcs],	[bp+8]
	mov	ax,	[bp+10]
	or	ax,	eflags.if
	mov	[cs:roottss.rfl],	ax
	mov	ax,	ds,	cs
	;mov	ax,	es,	[cs:roottss.rds]
	mov	ax,	ss,	microkernelstack
	mov		sp,	microkernelsp
	push	di	si	dx
	iret
KiQuantumBegin:
	cli
	;
	or	[cs:roottss.rfl],	eflags.if
	mov	ax,	ss,	[cs:roottss.rss]
	mov		sp,	[cs:roottss.rsp]
	mov	ax,	[cs:roottss.rfl]
	or	ax,	0x200
	push	ax
	push	[cs:roottss.rcs]
	push	[cs:roottss.rip]
	mov	ax,	ds,	[cs:roottss.rds]
        mov	ax,	es,	[cs:roottss.res]
        mov		ax,	[cs:roottss.rax]
        mov		bx,	[cs:roottss.rbx]
        mov		cx,	[cs:roottss.rcx]
        mov		dx,	[cs:roottss.rdx]
        mov		si,	[cs:roottss.rsi]
        mov		di,	[cs:roottss.rdi]
        mov		bp,	[cs:roottss.rbp]
	iret
KeSwitchStack:
	cli
	mov	ax,	[cs:roottss.kss]
	mov	cx,	ss
	mov	ss,	ax
	lock	xchg	sp,	[cs:roottss.ksp]
	mov	[cs:roottss.kss], cx
	iret
ExSyscall:
	iret
KeMultiplex:
	mov	ax,	1
	mov	dx,	1
	mov	cx,	0
	iret
DbgMultiplex: ;void (al int callnumber)
	mov	ah,	0	;movzx ax, al
	shl	ax,	1	;offset in table
	xchg	ax,	bx	;now offset is in bx
	mov	bx,	[cs:bx+.callstable]	;now bx have ea
	xchg	ax,	bx	;now we restored bx
	call	ax		;now we call function transparently
	iret
DbgMultiplex.callstable:
	multiline intDbgGetVersion		= 0	,dw DbgGetVersion
	multiline intDbgInitialize		= 1	,dw DbgInitialize
	multiline 				   	,dw KiDummyNearFunction
	multiline 				   	,dw KiDummyNearFunction
	multiline 				   	,dw KiDummyNearFunction
	multiline 				   	,dw KiDummyNearFunction
	multiline 				   	,dw KiDummyNearFunction
	multiline 				   	,dw KiDummyNearFunction
	multiline intDbgPutch			= 8	,dw DbgPutch
	multiline intDbgPuts			= 9	,dw DbgPuts
	multiline intDbgPutchRaw		= 10	,dw DbgPutchRaw
	multiline intDbgPrintUint16Hex		= 11	,dw DbgPrintUint16Hex
	multiline intDbgPrintUint8HexPrefixless = 12	,dw DbgPrintUint8HexPrefixless
	multiline intDbgGetch			= 13	,dw DbgGetch
DbgGetVersion:
	mov	ax,	0*256+1
	mov	dx,	1
	mov	cx,	0xBEEF
	ret
DbgGetch:
	mov	dx,	0x3FD
@@:	in	al,	dx
	test	al,	1
	jz	@b
	mov	dx,	0x3F8
	in	al,	dx
	ret
DbgInitialize:
	multiline <mov dx,0x3F8+1>,<mov al,0x00>,<out dx,al> ;disable interrupts
	multiline <mov dx,0x3F8+3>,<mov al,0x80>,<out dx,al> ;enable DLAB (it is sign of reg.3)
	multiline <mov dx,0x3F8+0>,<mov al,0x0C>,<out dx,al> ;baud rate divisor low
	multiline <mov dx,0x3F8+1>,<mov al,0x00>,<out dx,al> ;baud rate divisor hi
	multiline <mov dx,0x3F8+3>,<mov al,0x03>,<out dx,al> ;8n1
	multiline <mov dx,0x3F8+2>,<mov al,0x87>,<out dx,al> ;enable and flush queues, int when 8 rx bytes
	multiline <mov dx,0x3F8+4>,<mov al,0x0F>,<out dx,al> ;set it in normal operation mode
	;multiline <mov dx,0x3F8+1>,<mov al,0x0b>,<out dx,al> ;enable interrupts
	ret
KiDummyNearFunction:	
	ret
microkerneldata:
	roottss:	
	.tid	dw 0
	.pid	dw 0
	.t_parent dw 0
	.p_parent dw 0
	.state	dw 0
	.prio	dw 0
	.rax	dw 0
	.rbx	dw 0
	.rcx	dw 0
	.rdx	dw 0
	.rsi	dw 0
	.rdi	dw 0
	.rbp	dw 0
	.rsp	dw 0
	.rcs	dw 0
	.rds	dw 0
	.res	dw 0
	.rss	dw 0
	.rfl	dw 0
	.rip	dw 0
	.kss	dw 0
	.ksp	dw 0
	.wxpos	dw 0
	.wypos	dw 0
	.wxsize	dw 0
	.wysize	dw 0
	.whdr	dw 0
		dw 0
	.name	db '           '
	.end:
	.size = roottss.end - roottss
	db 128 dup (0)
	.tssbase	dw 0
processtable:	
	.tid	dw 0
	.pid	dw 0
	.t_parent dw 0
	.p_parent dw 0
	.state	dw 0
	kProcess.state.free		= 0	;Slot is free
	kProcess.state.running		= 1	;Running (reserved for future multiprocessor support)
	kProcess.state.runnable		= 2	;Runnable/running
	kProcess.state.suspended	= 3	;Suspended
	kProcess.state.suspended_wevent	= 4	;Suspended waiting event
	kProcess.state.suspended_wsignal = 5	;Suspended waiting signal
	kProcess.state.suspended_sleeping = 6	;Suspended sleeping
	kProcess.state.terminating	= 7	;Termination queued
	kProcess.state.wevent		= 8	;Waiting event
	kProcess.state.wsignal		= 9	;Waiting signal
	kProcess.state.sleeping		= 10	;Sleeping
	.prio	dw 0
	.rax	dw 0
	.rbx	dw 0
	.rcx	dw 0
	.rdx	dw 0
	.rsi	dw 0
	.rdi	dw 0
	.rbp	dw 0
	.rsp	dw 0
	.rcs	dw 0
	.rds	dw 0
	.res	dw 0
	.rss	dw 0
	.rfl	dw 0
	.rip	dw 0
	.kss	dw 0
	.ksp	dw 0
	.wxpos	dw 0
	.wypos	dw 0
	.wxsize	dw 0
	.wysize	dw 0
	.whdr	dw 0
		dw 0
	.name	db '           '
	display	'kProcess.size = '
	display	'0'+(($-processtable)/ 100)
	display	'0'+((($-processtable)/ 10) mod 10)
	display	'0'+(($-processtable) mod 10)
	kProcess.size	= 128
	db	kProcess.size*255 dup 0
microkerneldata.end:
microkerneludata:
microkerneludata.end:
microkernel.end:
;--------------------------------------------------------------
;-------------------------- Executive -------------------------
;--------------------------------------------------------------
	segment	executiveflat
ExMultiplex:
	mov	ah,	0	;movzx ax, al
	shl	ax,	1	;offset in table
	xchg	ax,	bx	;now offset is in bx
	mov	bx,	[cs:bx+.callstable]	;now bx have ea
	xchg	ax,	bx	;now we restored bx
	call	ax		;now we call function transparently
	iret			;the only problem is that ax is destroyed
.callstable:
	multiline intExGetVersion	= 0 	,dw ExGetVersion
	multiline intExInitialize	= 1 	,dw ExInitialize
	multiline 			 	,dw ExDummyNearFunction
	multiline 			 	,dw ExDummyNearFunction
	multiline 			 	,dw ExDummyNearFunction
	multiline 			 	,dw ExDummyNearFunction
	multiline 			 	,dw ExDummyNearFunction
	multiline 			 	,dw ExDummyNearFunction
	multiline 			 	,dw ExDummyNearFunction
ExInitialize:
	mov	ax,	0
	ret
ExGetVersion:
	mov	ax,	0*256+1
	mov	dx,	1
	mov	cx,	1337
	ret
ExDummyNearFunction:	
	ret
	
;--------------------------------------------------------------
;----------------------- Stack segments -----------------------
;--------------------------------------------------------------
	segment	microkernelstack
	dw 256 dup (?)
microkernelsp:
	segment	microkernelkstack
	dw 256 dup (?)
microkernelksp:
	segment	microkernelkstack2
	dw 256 dup (?)
microkernelksp2:

;--------------------------------------------------------------
;-------------------------- Graphics --------------------------
;--------------------------------------------------------------
;    Changing video mode is done via reloading video driver
	segment	gdicode
GdiMultiplex:
	mov	ah,	0	;movzx ax, al
	shl	ax,	1	;offset in table
	xchg	ax,	bx	;now offset is in bx
	mov	bx,	[cs:bx+.callstable]	;now bx have ea
	xchg	ax,	bx	;now we restored bx
	call	ax		;now we call function transparently
	iret
.callstable:
	multiline intGdiGetVersion	= 0,	dw GdiGetVersion		
	multiline intGdiInitialize	= 1,	dw GdiInitialize
	multiline 			,	dw GdiDummyNearFunction
	multiline 			,	dw GdiDummyNearFunction
	multiline 			,	dw GdiDummyNearFunction
	multiline 			,	dw GdiDummyNearFunction
	multiline 			,	dw GdiDummyNearFunction
	multiline 			,	dw GdiDummyNearFunction
	multiline intGdiGetResolutionX	= 8,	dw GdiGetResolutionX
	multiline intGdiGetResolutionY	= 9,	dw GdiGetResolutionY
	multiline intGdiGetColorDepth	= 10,	dw GdiGetColorDepth
	multiline intGdiPutPixel        = 11,	dw GdiPutPixel
	multiline intGdiGetPixel        = 12,	dw GdiGetPixel
GdiDummyNearFunction:	
	ret
GdiGetVersion:
	mov	ax,	1
	mov	dx,	1
	mov	cx,	0x9999
	ret
GdiInitialize:
	mov	al,	intVdInitialize
	int	intVdMultiplex
	ret
GdiGetResolutionX:
	mov	al,	intVdGetResolutionX
	int	intVdMultiplex
	ret
GdiGetResolutionY:
	mov	al,	intVdGetResolutionY
	int	intVdMultiplex
	ret
GdiGetColorDepth:
	mov	al,	intVdGetColorDepth
	int	intVdMultiplex
	ret
GdiPutPixel:
;DX:AX |31-24|23-12|11-0|
;	|	|	'- X 
;	|	`- Y
;	`- color
	mov	al,	intVdPutPixel
	int	intVdMultiplex
	ret
GdiGetPixel:
	mov	al,	intVdGetPixel
	int	intVdMultiplex
	ret
;---------------------------------------------------------
;-----------------------Videodriver-----------------------
;---------------------------------------------------------
	segment	vdcode
VdMultiplex:
	mov	ah,	0	;movzx ax, al
	shl	ax,	1	;offset in table
	xchg	ax,	bx	;now offset is in bx
	mov	bx,	[cs:bx+.callstable]	;now bx have ea
	xchg	ax,	bx	;now we restored bx
	call	ax		;now we call function transparently
	iret
.callstable:
	multiline 			   ,	dw VdDummyNearFunction
	multiline intVdInitialize	= 1,	dw VdInitialize
	multiline 			   ,	dw VdDummyNearFunction
	multiline 			   ,	dw VdDummyNearFunction
	multiline 			   ,	dw VdDummyNearFunction
	multiline 			   ,	dw VdDummyNearFunction
	multiline 			   ,	dw VdDummyNearFunction
	multiline 			   ,	dw VdDummyNearFunction
	multiline intVdGetResolutionX	= 8,	dw VdGetResolutionX
	multiline intVdGetResolutionY	= 9,	dw VdGetResolutionY
	multiline intVdGetColorDepth	= 10,	dw VdGetColorDepth
	multiline intVdPutPixel		= 11,	dw VdPutPixel
	multiline intVdGetPixel		= 12,	dw VdGetPixel
VdInitialize:
	mov	ax,	0x0003
	int	10h
	ret
VdDummyNearFunction:	
	ret
VdGetResolutionX:
	mov	ax,	80
	ret
VdGetResolutionY:
	mov	ax,	25
	ret
VdGetColorDepth:
	mov	ax,	16
	ret
VdPutPixel:	
.unpackargs:
	mov	al,	dh	;color -> ax
	mov	ah,	0
	mov	dh,	dl	;y -> dx
	mov	dl,	ch
	shr	dx,	1
	shr	dx,	1
	shr	dx,	1
	shr	dx,	1
	and	ch,	0x0f	;x -> cx
	xchg	cx,	dx	;cx <> dx
.threearg:	;void (dx int x, cx int y, ax int color);
	push	si	bx	es
	xchg	cx,	ax	;ax-y,cx-color
	mov	bl,	160	;ax-part.offset
	mul	bl
	mov	bx,	ax	;bx-part.offset
	add	bx,	dx	;bx-full offset
	add	bx,	dx
	mov	ax,	es,	0xB800	;es:bx - full offset
	and	cx,	15	;clamp color
	mov	si,	cx	
	mov	al,	[cs:.chars+si]	;fetch corresponding char
	mov	[es:bx],	al	;write it
	pop	es	bx	si
	ret
.chars:	db	" .+*",7,4,3,6,"#@$",2,176,177,178,219
VdGetPixel:
	mov	ax,	0
	ret
;--------------------------------------------------------------
;------------------------ Bootstrapper ------------------------
;--------------------------------------------------------------
	segment bootstrapperflat
bootstrapperentry:
	push	ds
	intKeInitialization	= 91h
	mov	ax,	0x2591		;setintvect
	mov	dx,	ds,	microkernelflat
	mov	dx,	KeInitialization
	int	21h
	int	0x91			;do kernel init.
	intGdiMultiplex	= 0xA2		;hook modules
	mov	ax,	0x25A2
	mov	dx,	ds,	gdicode
	mov	dx,	GdiMultiplex
	int	21h
	intVdMultiplex	= 0xA4
	mov	ax,	0x25A4
	mov	dx,	ds,	vdcode
	mov	dx,	VdMultiplex
	int	21h
	pop	ds
	int	0x91			;do second kernel init.
					;it will also init modules
	int3				;trigger trap
.askmt:	mov	es,	cs
	mov	dx,	szDosecondinitprompt
	mov	al,	intDbgPuts
	int	intDbgMultiplex
	mov	ah,	0
	int	16h
	cmp	al,	'Y'
	je	.domt
	cmp	al,	'y'
	je	.domt
	cmp	al,	'N'
	je	.nomt
	cmp	al,	'n'
	je	.nomt
	jmp	.askmt
.domt:	int	0x91			;do third kernel init. 
					;which will start multitasking
.nomt:	mov	ax,	0x3100
	mov	dx,	bootstrapperflat-microkernelflat+16
	int	21h
szDosecondinitprompt:	db 13,10,'Start multitasking? (y/n)',0