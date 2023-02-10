.architecture 8bitPipeline 
;			       ,,...........,,
;			       ::Description::
;			       '''''''''''''''
foldstart
;		The intention is high-speed Z80 emulator for writing programs 
;	primairly in Z80 assembly using pre-existing tools. The 65C02 likely
;	would have been a better choice, but Z80 have few advantages - ease of
;	programming, CP/M as a platform for development on and for, as well as
;	tens and hundreds of megabytes of CP/M 2.2 software.
;	So i chose Z80.
;		IMPORTANT NOTICES:
;	Addresses in Z80 programs are threated as PHYSICAL addresses. This
;	means that programs are running in <b>native address space</b>. 
;	This <b>must</b> be taken into account when writing software.
;		Not all opcodes will be supported, yet some from Z180 will be.
;		Cycling will mismatch and vary from version to version. Probably 
;	later there will be fixed-cycle versions, running syncronously to 
;	horizontal refresh with defined and locked cycling.
;		About flags:
;	Extracting multiple hardware flags is painful, and some are absent.
;	So some of the flags wont even remotely be present - the half-carry,
;	so DAA would not be present as well.
;	Implemented flags:
;		    .-+-- On Z80, BOTH Overflow and Parity are needed,
;		    | |	  as well as they are squeezed into one bit.
;	  s z - H - v/P N c
;	  | | | | | | | | |
;	  | | | | | | | | '- Implemented
;	  | | | | | | | '- Implemented
;	  | | | | | | '-- Implemented
;	  | | | | | '-- Implemented
;	  | | '-'-'- Force-zeroed
;	  | '- Implemented
;	  '- Implemented
;
;==============================================================================
;				 ,,............
;				 ::Memory map::
;				 ''''''''''''''
;			Shared between Z80 and Native code!
;		+------+-------------------------------------------+
;		| 0000 | CP/M Zeropage & Native applications base  |
;		| 0100 | CP/M TPA				   |
;		| 4000?| CP/M CCP (command.com)			   |
;		| 5000?| CP/M BDOS (msdos.sys)			   |
;		| 6000?| CP/M BIOS (io.sys)			   |
;		|68/90?| Z80 <-> PCPU layer - the hispeed emulator |
;		| 8800 | End of ram				   |
;		| 8B00 | VGA MMIO				   |
;		| 8C00 | VGA Palette RAM			   |
;		| 9000 | VGA Sprites data			   |
;		| A000 | VGA Tile data				   |
;		| C000 | VGA Framebuffer			   |
;		| E000 | Native monitor				   |
;		| F800 | Native stack in Shadow RAM		   |
;		| FFFF | End of address space			   |
;		+------+-------------------------------------------+
foldend
;				    ,,.......,,
;				    ::Headers::
;				    '''''''''''
foldstart
.entry entry
.export 0x0000 TopRomAddress
.origin 0x6800
.segment DataRO
foldend
;			,,.............................,,
;			::Constants and constant arrays::
;			'''''''''''''''''''''''''''''''''
msg_terminated:			db "Application terminated",13,10,0
msg_unimplemented_opcode:	db "[FATAL]: Unimplemented opcode!",13,10,0
msg_undefined_opcode:		db "[WARNING]: Undefined opcode!",13,10,0
msg_sessionend:			db "Emulated program terminated.",13,10,0
msg_wrongentrypoint:		
	db "Type in Z80 program or CP/M BIOS+BDOS+RAMDisk first, then jump to $6816",13,10
	db "to start the emulator.",13,10,0
msg_debugopcode:
foldstart
	db "DEBUG LOG",13,10,0
	;PC:0000 AF :0000 BC :0000 DE :0000 HL :0000 IX:0000 IY:0000
	;	 AF':0000 BC':0000 DE':0000 HL':0000
	;	 s z - h - v n c
	db "PC:",0
	db " AF: ",0
	db " BC: ",0
	db " DE: ",0
	db " HL: ",0
	db " IX: ",0
	db " IY: ",0
	db " SP: ",0
	db 13,10,"OP:",0
	db " AF':",0
	db " BC':",0
	db " DE':",0
	db " HL':",0
	db " IR: ",0
	db " IFF:",0
	db " ",13,10,"	      ",0
	db "sSzZ-+hH-+vVnNcC"
	db 13,10,0
foldend
msg_nativedebug:
foldstart
	db "NATIVE DEBUG LOG",13,10,0
	db "PC:",0
	db " SP:",0
	db " CD:",0
	db " AB:",0
	db " SI:",0
	db " DI:",0
	db " RA:",0
	db " TX:",0
	db 13,10,0
foldend
.subsegment
.align	256
z80_signzeroparity: foldstart
	db  64,	 4,  4,	 0,  4,	 0,  0,	 4,  4,	 0,  0,	 4,  0,	 4,  4,	 0
	db   4,	 0,  0,	 4,  0,	 4,  4,	 0,  0,	 4,  4,	 0,  4,	 0,  0,	 4
	db   4,	 0,  0,	 4,  0,	 4,  4,	 0,  0,	 4,  4,	 0,  4,	 0,  0,	 4
	db   0,	 4,  4,	 0,  4,	 0,  0,	 4,  4,	 0,  0,	 4,  0,	 4,  4,	 0
	db   4,	 0,  0,	 4,  0,	 4,  4,	 0,  0,	 4,  4,	 0,  4,	 0,  0,	 4
	db   0,	 4,  4,	 0,  4,	 0,  0,	 4,  4,	 0,  0,	 4,  0,	 4,  4,	 0
	db   0,	 4,  4,	 0,  4,	 0,  0,	 4,  4,	 0,  0,	 4,  0,	 4,  4,	 0
	db   4,	 0,  0,	 4,  0,	 4,  4,	 0,  0,	 4,  4,	 0,  4,	 0,  0,	 4
	db 132,128,128,132,128,132,132,128,128,132,132,128,132,128,128,132
	db 128,132,132,128,132,128,128,132,132,128,128,132,128,132,132,128
	db 128,132,132,128,132,128,128,132,132,128,128,132,128,132,132,128
	db 132,128,128,132,128,132,132,128,128,132,132,128,132,128,128,132
	db 128,132,132,128,132,128,128,132,132,128,128,132,128,132,132,128
	db 132,128,128,132,128,132,132,128,128,132,132,128,132,128,128,132
	db 132,128,128,132,128,132,132,128,128,132,132,128,132,128,128,132
	db 128,132,132,128,132,128,128,132,132,128,128,132,128,132,132,128
foldend
;z80_signzero:
foldstart
;	db  64,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0
;	db   0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0
;	db   0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0
;	db   0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0
;	db   0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0
;	db   0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0
;	db   0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0
;	db   0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0,  0,	 0
;	db 128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128
;	db 128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128
;	db 128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128
;	db 128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128
;	db 128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128
;	db 128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128
;	db 128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128
;	db 128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128
foldend
.subsegment
z80_opcode_table: foldstart
;struct opcode {
;	void(*handling_routine)(__reg_c__ uint8_t);
;	// ^ This is quirky C we all love. This is the way pointers to functions 
;	// are defined - <returned type>"(*"<name>")("<function parameters>");"
;	uint8_t params;
;	uint8_t flags;
;}
;		   0/8	     1/9       2/A	 3/B	     4/C	5/D	  6/E	     7/F
;	00	NOP	!|LD BC,nn |LD (BC),A |INC BC	 |INC B	    !|DEC B   !|LD B,n	 !|RLCA	    |
;	08	EX AF,AF!|ADD HL,BC|LD A,(BC) |DEC BC	 |INC C	    !|DEC C   !|LD C,n	 !|RRCA	    |
;	10	DJNZ d	 |LD DE,nn |LD (DE),A |INC DE	 |INC D	    !|DEC D   !|LD D,n	 !|RLA	    |
;	18	JR d	!|ADD HL,DE|LD A,(DE) |DEC DE	 |INC E	    !|DEC E   !|LD E,n	 !|RRA	    |
;	20	JR NZ,d	!|LD HL,nn |LD (nn),HL|INC HL	 |INC H	    !|DEC H   !|LD H,n	 !|DAA	   X|
;	28	JR Z,d	!|ADD HL,HL|LD HL,(nn)|DEC HL	 |INC L	    !|DEC L   !|LD L,n	 !|CPL	    |
;	30	JR NC,d	!|LD SP,nn |LD (nn),A |INC SP	 |INC (HL)   |DEC (HL) |LD (HL),n!|SCF	    |
;	38	JR C,d	!|ADD HL,SP|LD A,(nn) |DEC SP	 |INC A	    !|DEC A   !|LD A,n	 !|CCF	    |
;	40	LD B,B	!|LD B,C  !|LD B,D   !|LD B,E	!|LD B,H    !|LD B,L  !|LD B,(HL) |LD B,A  !|
;	48	LD C,B	!|LD C,C  !|LD C,D   !|LD C,E	!|LD C,H    !|LD C,L  !|LD C,(HL) |LD C,A  !|
;	50	LD D,B	!|LD D,C  !|LD D,D   !|LD D,E	!|LD D,H    !|LD D,L  !|LD D,(HL) |LD D,A  !|
;	58	LD E,B	!|LD E,C  !|LD E,D   !|LD E,E	!|LD E,H    !|LD E,L  !|LD E,(HL) |LD E,A  !|
;	60	LD H,B	!|LD H,C  !|LD H,D   !|LD H,E	!|LD H,H    !|LD H,L  !|LD H,(HL) |LD H,A  !|
;	68	LD L,B	!|LD L,C  !|LD L,D   !|LD L,E	!|LD L,H    !|LD L,L  !|LD L,(HL) |LD L,A  !|
;	70	LD (HL),B!LD (HL),C!LD (HL),D!|LD (HL),E!|LD (HL),H !|LD (HL),L|HALT	 ?|LD (HL),A!
;	78	LD A,B	!|LD A,C  !|LD A,D   !|LD A,E	!|LD A,H    !|LD A,L  !|LD A,(HL) !LD A,A  !|
;	80	ADD A,B	!|ADD A,C !|ADD A,D  !|ADD A,E	!|ADD A,H   !|ADD A,L !|ADD A,(HL)!ADD A,A  |
;	88	ADC A,B	!|ADC A,C !|ADC A,D  !|ADC A,E	!|ADC A,H   !|ADC A,L !|ADC A,(HL)!ADC A,A  |
;	90	SUB A,B	!|SUB A,C !|SUB A,D  !|SUB A,E	!|SUB A,H   !|SUB A,L !|SUB A,(HL)!SUB A,A  |
;	98	SBC A,B	!|SBC A,C !|SBC A,D  !|SBC A,E	!|SBC A,H   !|SBC A,L !|SBC A,(HL)!SBC A,A  |
;	A0	AND B	!|AND C	  !|AND D    !|AND E	!|AND H	    !|AND L   !|AND (HL)  !AND A    |
;	A8	XOR B	!|XOR C	  !|XOR D    !|XOR E	!|XOR H	    !|XOR L   !|XOR (HL)  !XOR A    |
;	B0	OR B	!|OR C	  !|OR D     !|OR E	!|OR H	    !|OR L     |OR (HL)	  |OR A	    |
;	B8	CP B	 |CP C	   |CP D      |CP E	 |CP H	     |CP L     |CP (HL)	  |CP A	    |
;	C0	RET NZ	!|POP BC  !|JP NZ,nn !|JP nn	!|CALL NZ,nn!|PUSH BC !|ADD A,n	  |RST &00  |
;	C8	RET Z	!|RET	  !|JP Z,nn  !|[ESH]	 |CALL Z,nn !|CALL nn !|ADC A,n	  |RST &08  |
;	D0	RET NC	!|POP DE  !|JP NC,nn !|OUT (n),A |CALL NC,nn!|PUSH DE !|SUB A,n	  |RST &10  |
;	D8	RET C	!|EXX	  !|JP C,nn  !|IN A,(n)	 |CALL C,nn !|[IX]     |SBC A,n	  |RST &18  |
;	E0	RET PO	!|POP HL  !|JP PO,nn !|EX (SP),HL|CALL PO,nn!|PUSH HL !|AND n	  |RST &20  |
;	E8	RET PE	!|JP (HL)  |JP PE,nn !|EX DE,HL !|CALL PE,nn!|[EXT]   !|XOR A,n	  |RST &28  |
;	F0	RET P	!|POP AF  !|JP P,nn  !|DI	 |CALL P,nn !|PUSH AF !|OR n	  |RST &30  |
;	F8	RET M	!|LD SP,HL |JP M,nn  !|EI	 |CALL M,nn !|[IY]     |CP n	  |RST &38  |
foldmid
	dw	z80_opcode_nop,			0	;00 000	NOP
	dw	z80_opcode_mov_r16_i16,		0	;01 001 LD BC,nn 
	dw	z80_opcode_unimplemented,	0	;02 002 LD (BC),A 
	dw	z80_opcode_unimplemented,	0	;03 003 INC BC	  
	dw	z80_opcode_inc_r8,		0	;04 004 INC B	   
	dw	z80_opcode_dec_r8,		0	;05 005 DEC B	 
	dw	z80_opcode_mov_r8_i8,		0	;06 006 LD B,n	  
	dw	z80_opcode_unimplemented,	0	;07 007 RLCA	 
	dw	z80_opcode_ex_af_af,		1	;08 010 EX AF,AF 
	dw	z80_opcode_unimplemented,	1	;09 011 ADD HL,BC
	dw	z80_opcode_unimplemented,	1	;0A 012 LD A,(BC) 
	dw	z80_opcode_unimplemented,	1	;0B 013 DEC BC	  
	dw	z80_opcode_inc_r8,		1	;0C 014 INC C	   
	dw	z80_opcode_dec_r8,		1	;0D 015 DEC C	 
	dw	z80_opcode_mov_r8_i8,		1	;0E 016 LD C,n	  
	dw	z80_opcode_unimplemented,	1	;0F 017 RRCA	 
	dw	z80_opcode_unimplemented,	2	;10 020 DJNZ d	 
	dw	z80_opcode_mov_r16_i16,	2		;11 021 LD DE,nn 
	dw	z80_opcode_unimplemented,	2	;12 022 LD (DE),A 
	dw	z80_opcode_unimplemented,	2	;13 023 INC DE	  
	dw	z80_opcode_inc_r8,		2	;14 024 INC D	   
	dw	z80_opcode_dec_r8,		2	;15 025 DEC D	 
	dw	z80_opcode_mov_r8_i8,		2	;16 026 LD D,n	  
	dw	z80_opcode_unimplemented,	2	;17 027 RLA	 
	dw	z80_opcode_jr_a8,		3	;18 030 JR d	 
	dw	z80_opcode_unimplemented,	3	;19 031 ADD HL,DE
	dw	z80_opcode_unimplemented,	3	;1A 032 LD A,(DE) 
	dw	z80_opcode_unimplemented,	3	;1B 033 DEC DE	  
	dw	z80_opcode_inc_r8,		3	;1C 034 INC E	   
	dw	z80_opcode_dec_r8,		3	;1D 035 DEC E	 
	dw	z80_opcode_mov_r8_i8,		3	;1E 036 LD E,n	  
	dw	z80_opcode_unimplemented,	3	;1F 037 RRA	 
	dw	z80_opcode_jr_cc_a8,		0	;20 040 JR NZ,d	 
	dw	z80_opcode_mov_r16_i16,		4	;21 041 LD HL,nn 
	dw	z80_opcode_unimplemented,	4	;22 042 LD (nn),HL
	dw	z80_opcode_unimplemented,	4	;23 043 INC HL	  
	dw	z80_opcode_inc_r8,		4	;24 044 INC H	   
	dw	z80_opcode_dec_r8,		4	;25 045 DEC H	 
	dw	z80_opcode_mov_r8_i8,		4	;26 046 LD H,n	  
	dw	z80_opcode_unimplemented,	4	;27 047 DAA	 
	dw	z80_opcode_jr_cc_a8,		1	;28 050 JR Z,d	 
	dw	z80_opcode_unimplemented,	5	;29 051 ADD HL,HL
	dw	z80_opcode_unimplemented,	5	;2A 052 LD HL,(nn)
	dw	z80_opcode_unimplemented,	5	;2B 053 DEC HL	  
	dw	z80_opcode_inc_r8,		5	;2C 054 INC L	   
	dw	z80_opcode_dec_r8,		5	;2D 055 DEC L	 
	dw	z80_opcode_mov_r8_i8,		5	;2E 056 LD L,n	  
	dw	z80_opcode_unimplemented,	5	;2F 057 CPL	 
	dw	z80_opcode_jr_cc_a8,		2	;30 060 JR NC,d	 
	dw	z80_opcode_mov_littleendian_r16_i16,20	;31 061 LD SP,nn 
	dw	z80_opcode_unimplemented,	6	;32 062 LD (nn),A 
	dw	z80_opcode_unimplemented,	6	;33 063 INC SP	  
	dw	z80_opcode_unimplemented,	6	;34 064 INC (HL)   
	dw	z80_opcode_unimplemented,	6	;35 065 DEC (HL) 
	dw	z80_opcode_mov_m_i8,		6	;36 066 LD (HL),n 
	dw	z80_opcode_unimplemented,	6	;37 067 SCF	 
	dw	z80_opcode_jr_cc_a8,		3	;38 070 JR C,d	 
	dw	z80_opcode_unimplemented,	7	;39 071 ADD HL,SP
	dw	z80_opcode_unimplemented,	7	;3A 072 LD A,(nn) 
	dw	z80_opcode_unimplemented,	7	;3B 073 DEC SP	  
	dw	z80_opcode_inc_r8,		7	;3C 074 INC A	   
	dw	z80_opcode_dec_r8,		7	;3D 075 DEC A	 
	dw	z80_opcode_mov_r8_i8,		7	;3E 076 LD A,n	  
	dw	z80_opcode_unimplemented,	7	;3F 077 CCF	 
	dw	z80_opcode_mov_r8_r8,		00	;40 100 LD B,B	 
	dw	z80_opcode_mov_r8_r8,		01	;41 101 LD B,C	 
	dw	z80_opcode_mov_r8_r8,		02	;42 102 LD B,D	  
	dw	z80_opcode_mov_r8_r8,		03	;43 103 LD B,E	  
	dw	z80_opcode_mov_r8_r8,		04	;44 104 LD B,H	   
	dw	z80_opcode_mov_r8_r8,		05	;45 105 LD B,L	 
	dw	z80_opcode_mov_r8_m,		0	;46 106 LD B,(HL) 
	dw	z80_opcode_mov_r8_r8,		07	;47 107 LD B,A	 
	dw	z80_opcode_mov_r8_r8,		08	;48 110 LD C,B	 
	dw	z80_opcode_mov_r8_r8,		09	;49 111 LD C,C	 
	dw	z80_opcode_mov_r8_r8,		10	;4A 112 LD C,D	  
	dw	z80_opcode_mov_r8_r8,		11	;4B 113 LD C,E	  
	dw	z80_opcode_mov_r8_r8,		12	;4C 114 LD C,H	   
	dw	z80_opcode_mov_r8_r8,		13	;4D 115 LD C,L	 
	dw	z80_opcode_mov_r8_m,		1	;4E 116 LD C,(HL) 
	dw	z80_opcode_mov_r8_r8,		15	;4F 117 LD C,A	 
	dw	z80_opcode_mov_r8_r8,		16	;50 120 LD D,B	 
	dw	z80_opcode_mov_r8_r8,		17	;51 121 LD D,C	 
	dw	z80_opcode_mov_r8_r8,		18	;52 122 LD D,D	  
	dw	z80_opcode_mov_r8_r8,		19	;53 123 LD D,E	  
	dw	z80_opcode_mov_r8_r8,		20	;54 124 LD D,H	   
	dw	z80_opcode_mov_r8_r8,		21	;55 125 LD D,L	 
	dw	z80_opcode_mov_r8_m,		2	;56 126 LD D,(HL) 
	dw	z80_opcode_mov_r8_r8,		23	;57 127 LD D,A	 
	dw	z80_opcode_mov_r8_r8,		24	;58 130 LD E,B	 
	dw	z80_opcode_mov_r8_r8,		25	;59 131 LD E,C	 
	dw	z80_opcode_mov_r8_r8,		26	;5A 132 LD E,D	  
	dw	z80_opcode_mov_r8_r8,		27	;5B 133 LD E,E	  
	dw	z80_opcode_mov_r8_r8,		28	;5C 134 LD E,H	   
	dw	z80_opcode_mov_r8_r8,		29	;5D 135 LD E,L	 
	dw	z80_opcode_mov_r8_m,		3	;5E 136 LD E,(HL) 
	dw	z80_opcode_mov_r8_r8,		31	;5F 137 LD E,A	 
	dw	z80_opcode_mov_r8_r8,		32	;60 140 LD H,B	 
	dw	z80_opcode_mov_r8_r8,		33	;61 141 LD H,C	 
	dw	z80_opcode_mov_r8_r8,		34	;62 142 LD H,D	  
	dw	z80_opcode_mov_r8_r8,		35	;63 143 LD H,E	  
	dw	z80_opcode_mov_r8_r8,		36	;64 144 LD H,H	   
	dw	z80_opcode_mov_r8_r8,		37	;65 145 LD H,L	 
	dw	z80_opcode_mov_r8_m,		4	;66 146 LD H,(HL) 
	dw	z80_opcode_mov_r8_r8,		39	;67 147 LD H,A	 
	dw	z80_opcode_mov_r8_r8,		40	;68 150 LD L,B	 
	dw	z80_opcode_mov_r8_r8,		41	;69 151 LD L,C	 
	dw	z80_opcode_mov_r8_r8,		42	;6A 152 LD L,D	  
	dw	z80_opcode_mov_r8_r8,		43	;6B 153 LD L,E	  
	dw	z80_opcode_mov_r8_r8,		44	;6C 154 LD L,H	   
	dw	z80_opcode_mov_r8_r8,		45	;6D 155 LD L,L	 
	dw	z80_opcode_mov_r8_m,		5	;6E 156 LD L,(HL) 
	dw	z80_opcode_mov_r8_r8,		47	;6F 157 LD L,A	 
	dw	z80_opcode_mov_m_r8,		0	;70 160 LD (HL),B
	dw	z80_opcode_mov_m_r8,		1	;71 161 LD (HL),C
	dw	z80_opcode_mov_m_r8,		2	;72 162 LD (HL),D 
	dw	z80_opcode_mov_m_r8,		3	;73 163 LD (HL),E 
	dw	z80_opcode_mov_m_r8,		4	;74 164 LD (HL),H  
	dw	z80_opcode_mov_m_r8,		5	;75 165 LD (HL),L
	dw	z80_opcode_unimplemented,	0	;76 166 The troublesome HALT
	dw	z80_opcode_mov_m_r8,		7	;77 167 LD (HL),A
	dw	z80_opcode_mov_r8_r8,		56	;78 170 LD A,B	 
	dw	z80_opcode_mov_r8_r8,		57	;79 171 LD A,C	 
	dw	z80_opcode_mov_r8_r8,		58	;7A 172 LD A,D	  
	dw	z80_opcode_mov_r8_r8,		59	;7B 173 LD A,E	  
	dw	z80_opcode_mov_r8_r8,		60	;7C 174 LD A,H	   
	dw	z80_opcode_mov_r8_r8,		61	;7D 175 LD A,L	 
	dw	z80_opcode_mov_r8_m,		7	;7E 176 LD A,(HL) 
	dw	z80_opcode_mov_r8_r8,		63	;7F 177 LD A,A	 
	dw	z80_opcode_add_r8,		0	;80 200 ADD A,B	 
	dw	z80_opcode_add_r8,		1	;81 201 ADD A,C	 
	dw	z80_opcode_add_r8,		2	;82 202 ADD A,D	  
	dw	z80_opcode_add_r8,		3	;83 203 ADD A,E	  
	dw	z80_opcode_add_r8,		4	;84 204 ADD A,H	   
	dw	z80_opcode_add_r8,		5	;85 205 ADD A,L	 
	dw	z80_opcode_add_m8,		6	;86 206 ADD A,(HL)
	dw	z80_opcode_add_r8,		7	;87 207 ADD A,A	 
	dw	z80_opcode_adc_r8,		0	;88 210 ADC A,B	 
	dw	z80_opcode_adc_r8,		1	;89 211 ADC A,C	 
	dw	z80_opcode_adc_r8,		2	;8A 212 ADC A,D	  
	dw	z80_opcode_adc_r8,		3	;8B 213 ADC A,E	  
	dw	z80_opcode_adc_r8,		4	;8C 214 ADC A,H	   
	dw	z80_opcode_adc_r8,		5	;8D 215 ADC A,L	 
	dw	z80_opcode_adc_m8,		6	;8E 216 ADC A,(HL)
	dw	z80_opcode_adc_r8,		7	;8F 217 ADC A,A	 
	dw	z80_opcode_sub_r8,		0	;90 220 SUB A,B	 
	dw	z80_opcode_sub_r8,		1	;91 221 SUB A,C	 
	dw	z80_opcode_sub_r8,		2	;92 222 SUB A,D	  
	dw	z80_opcode_sub_r8,		3	;93 223 SUB A,E	  
	dw	z80_opcode_sub_r8,		4	;94 224 SUB A,H	   
	dw	z80_opcode_sub_r8,		5	;95 225 SUB A,L	 
	dw	z80_opcode_sub_m8,		6	;96 226 SUB A,(HL)
	dw	z80_opcode_sub_r8,		7	;97 227 SUB A,A	 
	dw	z80_opcode_sbb_r8,		0	;98 230 SBC A,B	 
	dw	z80_opcode_sbb_r8,		1	;99 231 SBC A,C	 
	dw	z80_opcode_sbb_r8,		2	;9A 232 SBC A,D	  
	dw	z80_opcode_sbb_r8,		3	;9B 233 SBC A,E	  
	dw	z80_opcode_sbb_r8,		4	;9C 234 SBC A,H	   
	dw	z80_opcode_sbb_r8,		5	;9D 235 SBC A,L	 
	dw	z80_opcode_sbb_m8,		6	;9E 236 SBC A,(HL)
	dw	z80_opcode_sbb_r8,		7	;9F 237 SBC A,A	 
	dw	z80_opcode_and_r8,		0	;A0 240 AND B	 
	dw	z80_opcode_and_r8,		1	;A1 241 AND C	 
	dw	z80_opcode_and_r8,		2	;A2 242 AND D	  
	dw	z80_opcode_and_r8,		3	;A3 243 AND E	  
	dw	z80_opcode_and_r8,		4	;A4 244 AND H	   
	dw	z80_opcode_and_r8,		5	;A5 245 AND L	 
	dw	z80_opcode_and_m8,		6	;A6 246 AND (HL)  
	dw	z80_opcode_and_r8,		7	;A7 247 AND A	 
	dw	z80_opcode_xor_r8,		0	;A8 250 XOR B	 
	dw	z80_opcode_xor_r8,		1	;A9 251 XOR C	 
	dw	z80_opcode_xor_r8,		2	;AA 252 XOR D	  
	dw	z80_opcode_xor_r8,		3	;AB 253 XOR E	  
	dw	z80_opcode_xor_r8,		4	;AC 254 XOR H	   
	dw	z80_opcode_xor_r8,		5	;AD 255 XOR L	 
	dw	z80_opcode_xor_m8,		6	;AE 256 XOR (HL)  
	dw	z80_opcode_xor_r8,		7	;AF 257 XOR A	 
	dw	z80_opcode_or_r8,		0	;B0 260 OR B	 
	dw	z80_opcode_or_r8,		1	;B1 261 OR C	 
	dw	z80_opcode_or_r8,		2	;B2 262 OR D	  
	dw	z80_opcode_or_r8,		3	;B3 263 OR E	  
	dw	z80_opcode_or_r8,		4	;B4 264 OR H	   
	dw	z80_opcode_or_r8,		5	;B5 265 OR L	 
	dw	z80_opcode_or_m8,		6	;B6 266 OR (HL)	  
	dw	z80_opcode_or_r8,		7	;B7 267 OR A	 
	dw	z80_opcode_cmp_r8,		0	;B8 270 CP B	 
	dw	z80_opcode_cmp_r8,		1	;B9 271 CP C	 
	dw	z80_opcode_cmp_r8,		2	;BA 272 CP D	  
	dw	z80_opcode_cmp_r8,		3	;BB 273 CP E	  
	dw	z80_opcode_cmp_r8,		4	;BC 274 CP H	   
	dw	z80_opcode_cmp_r8,		5	;BD 275 CP L	 
	dw	z80_opcode_cmp_m8,		6	;BE 276 CP (HL)	  
	dw	z80_opcode_cmp_r8,		7	;BF 277 CP A	 
	dw	z80_opcode_ret_cc_a16,		0	;C0 300 RET NZ	 
	dw	z80_opcode_pop_rp,		0	;C1 301 POP BC	 
	dw	z80_opcode_jmp_cc_a16,		0	;C2 302 JP NZ,nn  
	dw	z80_opcode_jmp_a16,		0	;C3 303 JP nn	  
	dw	z80_opcode_call_cc_a16,		0	;C4 304 CALL NZ,nn 
	dw	z80_opcode_push_rp,		0	;C5 305 PUSH BC	 
	dw	z80_opcode_add_i8,		0	;C6 306 ADD A,n	  
	dw	z80_opcode_rst_nn,		0x00	;C7 307 RST &00	 
	dw	z80_opcode_ret_cc_a16,		1	;C8 310 RET Z	 
	dw	z80_opcode_ret,			1	;C9 311	RET
	dw	z80_opcode_jmp_cc_a16,		1	;CA 312 JP Z,nn	  
	dw	z80_opcode_unimplemented,	1	;CB 313 [ESH]
	dw	z80_opcode_call_cc_a16,		1	;CC 314 CALL Z,nn  
	dw	z80_opcode_call_a16,		1	;CD 315	CALL nn
	dw	z80_opcode_sub_i8,		1	;CE 316 ADC A,n	  
	dw	z80_opcode_rst_nn,		0x08	;CF 317 RST &08	 
	dw	z80_opcode_ret_cc_a16,		2	;D0 320 RET NC	 
	dw	z80_opcode_pop_rp,		2	;D1 321 POP DE	 
	dw	z80_opcode_jmp_cc_a16,		2	;D2 322 JP NC,nn  
	dw	z80_opcode_unimplemented,	2	;D3 323 OUT (n),A 
	dw	z80_opcode_call_cc_a16,		2	;D4 324 CALL NC,nn 
	dw	z80_opcode_push_rp,		2	;D5 325 PUSH DE	 
	dw	z80_opcode_adc_i8,		2	;D6 326 SUB A,n	  
	dw	z80_opcode_rst_nn,		0x10	;D7 327 RST &10	 
	dw	z80_opcode_ret_cc_a16,		3	;D8 330 RET C	 
	dw	z80_opcode_exx,			3	;D9 331 EXX	 
	dw	z80_opcode_jmp_cc_a16,		3	;DA 332 JP C,nn	  
	dw	z80_opcode_unimplemented,	3	;DB 333 IN A,(n)  
	dw	z80_opcode_call_cc_a16,		3	;DC 334 CALL C,nn  
	dw	z80_opcode_unimplemented,	3	;DD 335 [IX]
	dw	z80_opcode_sbb_i8,		3	;DE 336 SBC A,n	  
	dw	z80_opcode_rst_nn,		0x18	;DF 337 RST &18	 
	dw	z80_opcode_ret_cc_a16,		4	;E0 340 RET PO	 
	dw	z80_opcode_pop_rp,		4	;E1 341 POP HL	 
	dw	z80_opcode_jmp_cc_a16,		4	;E2 342 JP PO,nn  
	dw	z80_opcode_unimplemented,	4	;E3 343 EX (SP),HL
	dw	z80_opcode_call_cc_a16,		4	;E4 344 CALL PO,nn 
	dw	z80_opcode_push_rp,		4	;E5 345 PUSH HL	 
	dw	z80_opcode_and_i8,		4	;E6 346 AND n	  
	dw	z80_opcode_rst_nn,		0x20	;E7 347 RST &20	 
	dw	z80_opcode_ret_cc_a16,		5	;E8 350 RET PE	 
	dw	z80_opcode_unimplemented,	5	;E9 351 JP (HL)	 
	dw	z80_opcode_jmp_cc_a16,		5	;EA 352 JP PE,nn  
	dw	z80_opcode_ex_de_hl,		5	;EB 353 EX DE,HL
	dw	z80_opcode_call_cc_a16,		5	;EC 354 CALL PE,nn 
	dw	z80_dispatcher_ext,		0	;ED 355 [EXT]
	dw	z80_opcode_xor_i8,		5	;EE 356 XOR A,n	  
	dw	z80_opcode_rst_nn,		0x28	;EF 357 RST &28	 
	dw	z80_opcode_ret_cc_a16,		6	;F0 360 RET P	 
	dw	z80_opcode_pop_rp,		6	;F1 361 POP AF	 
	dw	z80_opcode_jmp_cc_a16,		6	;F2 362 JP P,nn	  
	dw	z80_opcode_unimplemented,	6	;F3 363 DI	  
	dw	z80_opcode_call_cc_a16,		6	;F4 364 CALL P,nn  
	dw	z80_opcode_push_rp,		6	;F5 365 PUSH AF	 
	dw	z80_opcode_or_i8,		6	;F6 366 OR n	  
	dw	z80_opcode_rst_nn,		0x30	;F7 367 RST &30	 
	dw	z80_opcode_ret_cc_a16,		7	;F8 370 RET M	 
	dw	z80_opcode_mov_sp_hl,		7	;F9 371 LD SP,HL 
	dw	z80_opcode_jmp_cc_a16,		7	;FA 372 JP M,nn	  
	dw	z80_opcode_unimplemented,	7	;FB 373 EI	  
	dw	z80_opcode_call_cc_a16,		7	;FC 374 CALL M,nn 
	dw	z80_opcode_unimplemented,	7	;FD 375 [IY]
	dw	z80_opcode_cmp_i8,		7	;FE 376 CP n	  
	dw	z80_opcode_rst_nn,		0x38	;FF 377 RST &38	 
	dw	z80_opcode_unimplemented,	0	;100 400 For catching off-by one errors
foldend
z80_ext_opcode_table: foldstart	;ED-prefixed opcodes
;	       0/8	   1/9	       2/A	    3/B		 4/C	     5/D	  6/E	       7/F
;	00 |IN0 B,(nn) |OUT0 (nn),B|LEA BC,IX+d|LEA BC,IY+d|TST A,B    |	   |	       |LD BC,(HL) |
;	08 |IN0 C,(nn) |OUT0 (nn),C|	       |	   |TST A,C    |	   |	       |LD (HL),BC |
;	10 |IN0 D,(nn) |OUT0 (nn),D|LEA DE,IX+d|LEA DE,IY+d|TST A,D    |	   |	       |LD DE,(HL) |
;	18 |IN0 E,(nn) |OUT0 (nn),E|	       |	   |TST A,E    |	   |	       |LD (HL),DE |
;	20 |IN0 H,(nn) |OUT0 (nn),H|LEA HL,IX+d|LEA HL,IY+d|TST A,H    |	   |	       |LD HL,(HL) |
;	28 |IN0 L,(nn) |OUT0 (nn),L|	       |	   |TST A,L    |	   |	       |LD (HL),HL |
;	30 |	       |LD IY,(HL) |LEA IX,IX+d|LEA IY,IY+d|TST A,(HL) |	   |	       |LD IX,(HL) |
;	38 |IN0 A,(nn) |OUT0 (nn),A|	       |	   |TST A,A    |	   |LD (HL),IY |LD (HL),IX |
;	40 |IN B,(C)   |OUT (C),B  |SBC HL,BC  |LD (nn),BC |NEG	       |RETN	   |IM 0       |LD I,A	   |
;	48 |IN C,(C)   |OUT (C),C  |ADC HL,BC  |LD BC,(nn) |MLT BC     |RETI	   |im 0       |LD R,A	   |
;	50 |IN D,(C)   |OUT (C),D  |SBC HL,DE  |LD (nn),DE |LEA IX,IY+d|LEA IY,IX+d|IM 1       |LD A,I	   |
;	58 |IN E,(C)   |OUT (C),E  |ADC HL,DE  |LD DE,(nn) |MLT DE     |	   |IM 2       |LD A,R	   |
;	60 |IN H,(C)   |OUT (C),H  |SBC HL,HL  |LD (nn),HL |TST A,n    |PEA IX+d   |PEA IY+d   |RRD	   |
;	68 |IN L,(C)   |OUT (C),L  |ADC HL,HL  |LD HL,(nn) |MLT HL     |LD MB,A	   |LD A,MB    |RLD	   |
;	70 |IN F,(C)   |OUT (C),F  |SBC HL,SP  |LD (nn),SP |TSTIO n    |	   |SLP	       |ld i,i	   |
;	78 |IN A,(C)   |OUT (C),A  |ADC HL,SP  |LD SP,(nn) |MLT SP     |STMIX	   |RSMIX      |ld r,r	   |
;	80 |	       |	   |INIM       |OTIM	   |INI2       |	   |	       |	   |
;	88 |	       |	   |INDM       |OTDM	   |IND2       |	   |	       |	   |
;	90 |	       |	   |INIMR      |OTIMR	   |INI2R      |	   |	       |	   |
;	98 |	       |	   |INDMR      |OTDMR	   |IND2R      |	   |	       |	   |
;	A0 |LDI	       |CPI	   |INI	       |OTI	   |OTI2       |	   |	       |	   |
;	A8 |LDD	       |CPD	   |IND	       |OTD	   |OTI2R      |	   |	       |	   |
;	B0 |LDIR       |CPIR	   |INIR       |OTIR	   |OTD2       |	   |	       |	   |
;	B8 |LDDR       |CPDR	   |INDR       |OTDR	   |OTD2R      |	   |	       |	   |
;	C0 |	       |	   |INIRX      |OTIRX	   |	       |	   |	       |LD I,HL	   |
;	C8 |	       |	   |INDRX      |OTDRX	   |	       |	   |	       |	   |
;	D0 |NCALL a16  |NCALL HL   |	       |	   |	       |	   |	       |LD HL,I	   |
;	D8 |	       |	   |	       |	   |	       |	   |	       |	   |
;	E0 |LDZP B,(a8)|LDZP C,(a8)|LDZP D,(a8)|LDZP E,(a8)|LDZP H,(a8)|LDZP L,(a8)|LDZP F,(a8)|LDZP A,(a8)|
;	E8 |LDZP (a8),B|LDZP (a8),C|LDZP (a8),D|LDZP (a8),E|LDZP (a8),H|LDZP (a8),L|LDZP (a8),F|LDZP (a8),A|
;	F0 |LDDP B,(a8)|LDDP C,(a8)|LDDP D,(a8)|LDDP E,(a8)|LDDP H,(a8)|LDDP L,(a8)|LDDP F,(a8)|LDDP A,(a8)|
;	F8 |LDDP (a8),B|LDDP (a8),C|LDDP (a8),D|LDDP (a8),E|LDDP (a8),H|LDDP (a8),L|LDDP (a8),F|LDDP (a8),A|
foldmid
	dw z80_opcode_unimplemented,	0	;00	000	IN0 B,(nn) 
	dw z80_opcode_unimplemented,	0	;01	001	OUT0 (nn),B
	dw z80_opcode_unimplemented,	0	;02	002	LEA BC,IX+d
	dw z80_opcode_unimplemented,	0	;03	003	LEA BC,IY+d
	dw z80_opcode_unimplemented,	0	;04	004	TST A,B	   
	dw z80_opcode_unimplemented,	0	;05	005		   
	dw z80_opcode_unimplemented,	0	;06	006		   
	dw z80_opcode_unimplemented,	0	;07	007	LD BC,(HL) 
	dw z80_opcode_unimplemented,	0	;08	010	IN0 C,(nn) 
	dw z80_opcode_unimplemented,	0	;09	011	OUT0 (nn),C
	dw z80_opcode_unimplemented,	0	;0A	012		   
	dw z80_opcode_unimplemented,	0	;0B	013		   
	dw z80_opcode_unimplemented,	0	;0C	014	TST A,C	   
	dw z80_opcode_unimplemented,	0	;0D	015		   
	dw z80_opcode_unimplemented,	0	;0E	016		   
	dw z80_opcode_unimplemented,	0	;0F	017	LD (HL),BC 
	dw z80_opcode_unimplemented,	0	;10	020	IN0 D,(nn) 
	dw z80_opcode_unimplemented,	0	;11	021	OUT0 (nn),D
	dw z80_opcode_unimplemented,	0	;12	022	LEA DE,IX+d
	dw z80_opcode_unimplemented,	0	;13	023	LEA DE,IY+d
	dw z80_opcode_unimplemented,	0	;14	024	TST A,D	   
	dw z80_opcode_unimplemented,	0	;15	025		   
	dw z80_opcode_unimplemented,	0	;16	026		   
	dw z80_opcode_unimplemented,	0	;17	027	LD DE,(HL) 
	dw z80_opcode_unimplemented,	0	;18	030	IN0 E,(nn) 
	dw z80_opcode_unimplemented,	0	;19	031	OUT0 (nn),E
	dw z80_opcode_unimplemented,	0	;1A	032		   
	dw z80_opcode_unimplemented,	0	;1B	033		   
	dw z80_opcode_unimplemented,	0	;1C	034	TST A,E	   
	dw z80_opcode_unimplemented,	0	;1D	035		   
	dw z80_opcode_unimplemented,	0	;1E	036		   
	dw z80_opcode_unimplemented,	0	;1F	037	LD (HL),DE 
	dw z80_opcode_unimplemented,	0	;20	040	IN0 H,(nn) 
	dw z80_opcode_unimplemented,	0	;21	041	OUT0 (nn),H
	dw z80_opcode_unimplemented,	0	;22	042	LEA HL,IX+d
	dw z80_opcode_unimplemented,	0	;23	043	LEA HL,IY+d
	dw z80_opcode_unimplemented,	0	;24	044	TST A,H	   
	dw z80_opcode_unimplemented,	0	;25	045		   
	dw z80_opcode_unimplemented,	0	;26	046		   
	dw z80_opcode_unimplemented,	0	;27	047	LD HL,(HL) 
	dw z80_opcode_unimplemented,	0	;28	050	IN0 L,(nn) 
	dw z80_opcode_unimplemented,	0	;29	051	OUT0 (nn),L
	dw z80_opcode_unimplemented,	0	;2A	052		   
	dw z80_opcode_unimplemented,	0	;2B	053		   
	dw z80_opcode_unimplemented,	0	;2C	054	TST A,L	   
	dw z80_opcode_unimplemented,	0	;2D	055		   
	dw z80_opcode_unimplemented,	0	;2E	056		   
	dw z80_opcode_unimplemented,	0	;2F	057	LD (HL),HL 
	dw z80_opcode_unimplemented,	0	;30	060		   
	dw z80_opcode_unimplemented,	0	;31	061	LD IY,(HL) 
	dw z80_opcode_unimplemented,	0	;32	062	LEA IX,IX+d
	dw z80_opcode_unimplemented,	0	;33	063	LEA IY,IY+d
	dw z80_opcode_unimplemented,	0	;34	064	TST A,(HL) 
	dw z80_opcode_unimplemented,	0	;35	065		   
	dw z80_opcode_unimplemented,	0	;36	066		   
	dw z80_opcode_unimplemented,	0	;37	067	LD IX,(HL) 
	dw z80_opcode_unimplemented,	0	;38	070	IN0 A,(nn) 
	dw z80_opcode_unimplemented,	0	;39	071	OUT0 (nn),A
	dw z80_opcode_unimplemented,	0	;3A	072		   
	dw z80_opcode_unimplemented,	0	;3B	073		   
	dw z80_opcode_unimplemented,	0	;3C	074	TST A,A	   
	dw z80_opcode_unimplemented,	0	;3D	075		   
	dw z80_opcode_unimplemented,	0	;3E	076	LD (HL),IY 
	dw z80_opcode_unimplemented,	0	;3F	077	LD (HL),IX 
	dw z80_opcode_unimplemented,	0	;40	100	IN B,(C)   
	dw z80_opcode_unimplemented,	0	;41	101	OUT (C),B  
	dw z80_opcode_unimplemented,	0	;42	102	SBC HL,BC  
	dw z80_opcode_unimplemented,	0	;43	103	LD (nn),BC 
	dw z80_opcode_unimplemented,	0	;44	104	NEG	   
	dw z80_opcode_unimplemented,	0	;45	105	RETN	   
	dw z80_opcode_unimplemented,	0	;46	106	IM 0	   
	dw z80_opcode_unimplemented,	0	;47	107	LD I,A	   
	dw z80_opcode_unimplemented,	0	;48	110	IN C,(C)   
	dw z80_opcode_unimplemented,	0	;49	111	OUT (C),C  
	dw z80_opcode_unimplemented,	0	;4A	112	ADC HL,BC  
	dw z80_opcode_unimplemented,	0	;4B	113	LD BC,(nn) 
	dw z80_opcode_unimplemented,	0	;4C	114	MLT BC	   
	dw z80_opcode_unimplemented,	0	;4D	115	RETI	   
	dw z80_opcode_unimplemented,	0	;4E	116	im 0	   
	dw z80_opcode_unimplemented,	0	;4F	117	LD R,A	   
	dw z80_opcode_unimplemented,	0	;50	120	IN D,(C)   
	dw z80_opcode_unimplemented,	0	;51	121	OUT (C),D  
	dw z80_opcode_unimplemented,	0	;52	122	SBC HL,DE  
	dw z80_opcode_unimplemented,	0	;53	123	LD (nn),DE 
	dw z80_opcode_unimplemented,	0	;54	124	LEA IX,IY+d
	dw z80_opcode_unimplemented,	0	;55	125	LEA IY,IX+d
	dw z80_opcode_unimplemented,	0	;56	126	IM 1	   
	dw z80_opcode_unimplemented,	0	;57	127	LD A,I	   
	dw z80_opcode_unimplemented,	0	;58	130	IN E,(C)   
	dw z80_opcode_unimplemented,	0	;59	131	OUT (C),E  
	dw z80_opcode_unimplemented,	0	;5A	132	ADC HL,DE  
	dw z80_opcode_unimplemented,	0	;5B	133	LD DE,(nn) 
	dw z80_opcode_unimplemented,	0	;5C	134	MLT DE	   
	dw z80_opcode_unimplemented,	0	;5D	135		   
	dw z80_opcode_unimplemented,	0	;5E	136	IM 2	   
	dw z80_opcode_unimplemented,	0	;5F	137	LD A,R	   
	dw z80_opcode_unimplemented,	0	;60	140	IN H,(C)   
	dw z80_opcode_unimplemented,	0	;61	141	OUT (C),H  
	dw z80_opcode_unimplemented,	0	;62	142	SBC HL,HL  
	dw z80_opcode_unimplemented,	0	;63	143	LD (nn),HL 
	dw z80_opcode_unimplemented,	0	;64	144	TST A,n	   
	dw z80_opcode_unimplemented,	0	;65	145	PEA IX+d   
	dw z80_opcode_unimplemented,	0	;66	146	PEA IY+d   
	dw z80_opcode_unimplemented,	0	;67	147	RRD	   
	dw z80_opcode_unimplemented,	0	;68	150	IN L,(C)   
	dw z80_opcode_unimplemented,	0	;69	151	OUT (C),L  
	dw z80_opcode_unimplemented,	0	;6A	152	ADC HL,HL  
	dw z80_opcode_unimplemented,	0	;6B	153	LD HL,(nn) 
	dw z80_opcode_unimplemented,	0	;6C	154	MLT HL	   
	dw z80_opcode_unimplemented,	0	;6D	155	LD MB,A	   
	dw z80_opcode_unimplemented,	0	;6E	156	LD A,MB	   
	dw z80_opcode_unimplemented,	0	;6F	157	RLD	   
	dw z80_opcode_unimplemented,	0	;70	160	IN F,(C)   Note that IN (HL),(C) becomes reality!
	dw z80_opcode_unimplemented,	0	;71	161	OUT (C),F  
	dw z80_opcode_unimplemented,	0	;72	162	SBC HL,SP  
	dw z80_opcode_unimplemented,	0	;73	163	LD (nn),SP 
	dw z80_opcode_unimplemented,	0	;74	164	TSTIO n	   
	dw z80_opcode_unimplemented,	0	;75	165		   
	dw z80_opcode_unimplemented,	0	;76	166	SLP	   
	dw z80_opcode_unimplemented,	0	;77	167	ld i,i	   
	dw z80_opcode_unimplemented,	0	;78	170	IN A,(C)   
	dw z80_opcode_unimplemented,	0	;79	171	OUT (C),A  
	dw z80_opcode_unimplemented,	0	;7A	172	ADC HL,SP  
	dw z80_opcode_unimplemented,	0	;7B	173	LD SP,(nn) 
	dw z80_opcode_unimplemented,	0	;7C	174	MLT SP	   
	dw z80_opcode_unimplemented,	0	;7D	175	STMIX	   
	dw z80_opcode_unimplemented,	0	;7E	176	RSMIX	   
	dw z80_opcode_unimplemented,	0	;7F	177	ld r,r	   
	dw z80_opcode_unimplemented,	0	;80	200		   
	dw z80_opcode_undefined,	0	;81	201		   
	dw z80_opcode_unimplemented,	0	;82	202	INIM	   
	dw z80_opcode_unimplemented,	0	;83	203	OTIM	   
	dw z80_opcode_unimplemented,	0	;84	204	INI2	   
	dw z80_opcode_undefined,	0	;85	205		   
	dw z80_opcode_undefined,	0	;86	206		   
	dw z80_opcode_undefined,	0	;87	207		   
	dw z80_opcode_undefined,	0	;88	210		   
	dw z80_opcode_undefined,	0	;89	211		   
	dw z80_opcode_unimplemented,	0	;8A	212	INDM	   
	dw z80_opcode_unimplemented,	0	;8B	213	OTDM	   
	dw z80_opcode_unimplemented,	0	;8C	214	IND2	   
	dw z80_opcode_undefined,	0	;8D	215		   
	dw z80_opcode_undefined,	0	;8E	216		   
	dw z80_opcode_undefined,	0	;8F	217		   
	dw z80_opcode_undefined,	0	;90	220		   
	dw z80_opcode_undefined,	0	;91	221		   
	dw z80_opcode_unimplemented,	0	;92	222	INIMR	   
	dw z80_opcode_unimplemented,	0	;93	223	OTIMR	   
	dw z80_opcode_unimplemented,	0	;94	224	INI2R	   
	dw z80_opcode_undefined,	0	;95	225		   
	dw z80_opcode_undefined,	0	;96	226		   
	dw z80_opcode_undefined,	0	;97	227		   
	dw z80_opcode_undefined,	0	;98	230		   
	dw z80_opcode_undefined,	0	;99	231		   
	dw z80_opcode_unimplemented,	0	;9A	232	INDMR	   
	dw z80_opcode_unimplemented,	0	;9B	233	OTDMR	   
	dw z80_opcode_unimplemented,	0	;9C	234	IND2R	   
	dw z80_opcode_undefined,	0	;9D	235		   
	dw z80_opcode_undefined,	0	;9E	236		   
	dw z80_opcode_undefined,	0	;9F	237		   
	dw z80_opcode_unimplemented,	0	;A0	240	LDI	   
	dw z80_opcode_unimplemented,	0	;A1	241	CPI	   
	dw z80_opcode_unimplemented,	0	;A2	242	INI	   
	dw z80_opcode_unimplemented,	0	;A3	243	OTI	   
	dw z80_opcode_unimplemented,	0	;A4	244	OTI2	   
	dw z80_opcode_undefined,	0	;A5	245		   
	dw z80_opcode_undefined,	0	;A6	246		   
	dw z80_opcode_undefined,	0	;A7	247		   
	dw z80_opcode_unimplemented,	0	;A8	250	LDD	   
	dw z80_opcode_unimplemented,	0	;A9	251	CPD	   
	dw z80_opcode_unimplemented,	0	;AA	252	IND	   
	dw z80_opcode_unimplemented,	0	;AB	253	OTD	   
	dw z80_opcode_unimplemented,	0	;AC	254	OTI2R	   
	dw z80_opcode_undefined,	0	;AD	255		   
	dw z80_opcode_undefined,	0	;AE	256		   
	dw z80_opcode_undefined,	0	;AF	257		   
	dw z80_opcode_unimplemented,	0	;B0	260	LDIR	   
	dw z80_opcode_unimplemented,	0	;B1	261	CPIR	   
	dw z80_opcode_unimplemented,	0	;B2	262	INIR	   
	dw z80_opcode_unimplemented,	0	;B3	263	OTIR	   
	dw z80_opcode_unimplemented,	0	;B4	264	OTD2	   
	dw z80_opcode_undefined,	0	;B5	265		   
	dw z80_opcode_undefined,	0	;B6	266		   
	dw z80_opcode_undefined,	0	;B7	267		   
	dw z80_opcode_unimplemented,	0	;B8	270	LDDR	   
	dw z80_opcode_unimplemented,	0	;B9	271	CPDR	   
	dw z80_opcode_unimplemented,	0	;BA	272	INDR	   
	dw z80_opcode_unimplemented,	0	;BB	273	OTDR	   
	dw z80_opcode_unimplemented,	0	;BC	274	OTD2R	   
	dw z80_opcode_undefined,	0	;BD	275		   
	dw z80_opcode_undefined,	0	;BE	276		   
	dw z80_opcode_undefined,	0	;BF	277		   
	dw z80_opcode_undefined,	0	;C0	300		   
	dw z80_opcode_undefined,	0	;C1	301		   
	dw z80_opcode_unimplemented,	0	;C2	302	INIRX	   
	dw z80_opcode_unimplemented,	0	;C3	303	OTIRX	   
	dw z80_opcode_undefined,	0	;C4	304		   
	dw z80_opcode_undefined,	0	;C5	305		   
	dw z80_opcode_undefined,	0	;C6	306		   
	dw z80_opcode_unimplemented,	0	;C7	307	LD I,HL	   
	dw z80_opcode_undefined,	0	;C8	310		   
	dw z80_opcode_undefined,	0	;C9	311		   
	dw z80_opcode_unimplemented,	0	;CA	312	INDRX	   
	dw z80_opcode_unimplemented,	0	;CB	313	OTDRX	   
	dw z80_opcode_undefined,	0	;CC	314		   
	dw z80_opcode_undefined,	0	;CD	315		   
	dw z80_opcode_undefined,	0	;CE	316		   
	dw z80_opcode_undefined,	0	;CF	317		   
	dw z80_opcode_ncall_a16,	0	;D0	320	NCALL a16	Native call commands
	dw z80_opcode_ncall_hl,		0	;D1	321	NCALL HL	   
	dw z80_opcode_undefined,	0	;D2	322		   
	dw z80_opcode_undefined,	0	;D3	323		   
	dw z80_opcode_undefined,	0	;D4	324		   
	dw z80_opcode_undefined,	0	;D5	325		   
	dw z80_opcode_undefined,	0	;D6	326		   
	dw z80_opcode_unimplemented,	0	;D7	327	LD HL,I	   
	dw z80_opcode_undefined,	0	;D8	330		   
	dw z80_opcode_undefined,	0	;D9	331		   
	dw z80_opcode_undefined,	0	;DA	332		   
	dw z80_opcode_undefined,	0	;DB	333		   
	dw z80_opcode_undefined,	0	;DC	334		   
	dw z80_opcode_undefined,	0	;DD	335		   
	dw z80_opcode_undefined,	0	;DE	336		   
	dw z80_opcode_undefined,	0	;DF	337		   
	dw z80_opcode_unimplemented,	0	;E0	340	LDZP B,(a8)	The LDZP and LDDP 
	dw z80_opcode_unimplemented,	0	;E1	341	LDZP C,(a8)	were not present
	dw z80_opcode_unimplemented,	0	;E2	342	LDZP D,(a8)	neither on eZ80
	dw z80_opcode_unimplemented,	0	;E3	343	LDZP E,(a8)	nor on Z380, but
	dw z80_opcode_unimplemented,	0	;E4	344	LDZP H,(a8)	I would like to
	dw z80_opcode_unimplemented,	0	;E5	345	LDZP L,(a8)	implement them to
	dw z80_opcode_unimplemented,	0	;E6	346	LDZP F,(a8)	use some of eZ80
	dw z80_opcode_unimplemented,	0	;E7	347	LDZP A,(a8)	registers to not
	dw z80_opcode_unimplemented,	0	;E8	350	LDZP (a8),B	change memory bank
	dw z80_opcode_unimplemented,	0	;E9	351	LDZP (a8),C	for Z80 mode in
	dw z80_opcode_unimplemented,	0	;EA	352	LDZP (a8),D	fashion of x86's
	dw z80_opcode_unimplemented,	0	;EB	353	LDZP (a8),E	realmode segment
	dw z80_opcode_unimplemented,	0	;EC	354	LDZP (a8),H	registers, but to
	dw z80_opcode_unimplemented,	0	;ED	355	LDZP (a8),L	switch the page
	dw z80_opcode_unimplemented,	0	;EE	356	LDZP (a8),F	for direct page
	dw z80_opcode_unimplemented,	0	;EF	357	LDZP (a8),A	access.
	dw z80_opcode_unimplemented,	0	;F0	360	LDDP B,(a8)	They are also not
	dw z80_opcode_unimplemented,	0	;F1	361	LDDP C,(a8)	using XY, as a 
	dw z80_opcode_unimplemented,	0	;F2	362	LDDP D,(a8)	common method for
	dw z80_opcode_unimplemented,	0	;F3	363	LDDP E,(a8)	accessing structures
	dw z80_opcode_unimplemented,	0	;F4	364	LDDP H,(a8)	would do -
	dw z80_opcode_unimplemented,	0	;F5	365	LDDP L,(a8)	load XY with ptr.
	dw z80_opcode_unimplemented,	0	;F6	366	LDDP F,(a8)	to structure
	dw z80_opcode_unimplemented,	0	;F7	367	LDDP A,(a8)	(say, ZP) and use
	dw z80_opcode_unimplemented,	0	;F8	370	LDDP (a8),B	XY+d addressing
	dw z80_opcode_unimplemented,	0	;F9	371	LDDP (a8),C	to refer to the
	dw z80_opcode_unimplemented,	0	;FA	372	LDDP (a8),D	structure.
	dw z80_opcode_unimplemented,	0	;FB	373	LDDP (a8),E
	dw z80_opcode_unimplemented,	0	;FC	374	LDDP (a8),H
	dw z80_opcode_unimplemented,	0	;FD	375	LDDP (a8),L
	dw z80_opcode_unimplemented,	0	;FE	376	LDDP (a8),F
	dw z80_opcode_unimplemented,	0	;FF	377	LDDP (a8),A
foldend
z80_condcodes_to_flagmask:	;XOR and AND values for condition codes
	db	0x00,0x40	;NZ
	db	0xFF,0x40	;Z
	db	0x00,0x01	;NC
	db	0xFF,0x01	;C
	db	0x00,0x04	;PO=NV
	db	0xFF,0x04	;PE=V
	db	0x00,0x80	;P=NS
	db	0xFF,0x80	;M=S
	
	
	
.segment DataRW
;				,,.........,,
;				::Variables::
;				'''''''''''''
foldstart
init_sp:		dw 0	;SP to restore to return to monitor.
;Z80 Register set
.align 8
z80_registers:
z80_b:	db	0
z80_c:	db	0
z80_d:	db	0
z80_e:	db	0
z80_h:	db	0
z80_l:	db	0
z80_f:	db	0
z80_a:	db	0
z80_b2:	db	0
z80_c2:	db	0
z80_d2:	db	0
z80_e2:	db	0
z80_h2:	db	0
z80_l2:	db	0
z80_f2:	db	0
z80_a2:	db	0
z80_ix:;dw	0
z80_ixh: db	0
z80_ixl: db	0
z80_iy:;dw	0
z80_iyh: db	0
z80_iyl: db	0
z80_sp:	dw	0x4000		;End of CP/M TPA and start of CCP (aka command.com)
;z80_pc:	dw	0	;Is offloaded to SI, taking the RT intention
z80_i:	db	0
z80_r:	db	0
z80_iff:	db	0	;I'm not sure if IFF0/1 are needed
foldend
.segment Code
;				   ,,....,,
;				   ::Code::
;				   ''''''''
entry:	proc
	;nop		;Byte at $0 must be a nop, but I am loading to $7000.
			;Address $0 is taken care of by Z80 software.
	push	ra
	mov	si,	msg_wrongentrypoint
	call	uart_write
	pop	ra
	ret
	;nop
	endp
start:	proc
	push	ra
	mov	tx,	sp
	mov	ab,	tx
	sta	init_sp
	stb	init_sp+1
	call	main	
os_return:
	mov	si,	msg_sessionend
	call	uart_write
	lda	init_sp
	ldb	init_sp+1
	mov	tx,	ab
	mov	ra,	tx
	pop	ra
	;break
	ret
	endp
emuloop:	proc
	;Register allocation
	;RA	Native fast return address
	;SP	Native machine stack pointer
	;SI	Z80 PC
	;DI	gen. purpose
	;AB	gen. purpose
	;CD	gen. purpose
	push	ra
	mov	si,	0x100	;Start point for the emulator is CP/M TPA
emuloop.main:
	;native command		macro command		comment
	;---	--	--	===	==	=	-=-=-=-=-=-=-=-
	mov	tx,	z80_r	;inc	[z80_r]		;increment Z80
	mov	a,	[tx]				;refresh register
	inc	a
	mov	[tx],	a
	call	z80_opcode_debug			;do debug output
	;call	native_debug
	mov	c,	[si]	;movzx	c,	[si++]	;fetch Z80 command
	mov	d,	0
	inc	si
	mov	ab,	z80_opcode_table		;load table base
	clc			;sal	cd,	2	;compute table offset
	shl	c
	shl	d
	shl	c
	shl	d
	add	a,	c	;add	ab,	cd	;compute effective address (EA)
	add	b,	d
	mov	di,	ab				;place EA into DI
	mov	a,	[di]	;mov	ab,	[di++]	;fetch handler ptr from
	mov	tl,	a	;mov	tx,	ab	;opcode table into AB&TX
	inc	di
	mov	b,	[di]
	mov	th,	b
	inc	di
	mov	c,	[di]				;fetch handler parameter into C
	call	tx					;call handler
	jmp	emuloop.main
emuloop.ret:
	pop	ra
	ret
	endp
main:	proc
	push	ra
	call	emuloop
	pop	ra
	ret
	endp
z80_dispatcher_ext:	proc
	mov	tx,	z80_r	;inc	[z80_r]		;increment Z80
	mov	a,	[tx]				;refresh register
	inc	a
	mov	[tx],	a
	mov	c,	[si]	;movzx	c,	[si++]	;fetch Z80 command
	mov	d,	0
	inc	si
	mov	ab,	z80_ext_opcode_table		;load table base
	clc			;sal	cd,	2	;compute table offset
	shl	c
	shl	d
	shl	c
	shl	d
	add	a,	c	;add	ab,	cd	;compute effective address (EA)
	add	b,	d
	mov	di,	ab				;place EA into DI
	mov	a,	[di]	;mov	ab,	[di++]	;fetch handler ptr from
	mov	tl,	a	;mov	tx,	ab	;opcode table into AB&TX
	inc	di
	mov	b,	[di]
	mov	th,	b
	inc	di
	mov	c,	[di]				;fetch handler parameter into C
	jmp	tx					;call handler
	ret	
	endp
z80_opcode_nop:	proc
	;As opcode handlers are usually not calling other subrouties,
	;no additional enter and leave code is needed.
	nop;?		;lol
	ret
	endp
z80_opcode_mov_r8_i8:	proc
	;break
	;Parameters:
	;C	register number
	;Covers:
	;0n6:	006-076 w/o 066 (MOV [HL], I8)
	mov	ab,	z80_registers;lea	ab,	[ab+c]
	add	a,	c
	incc	b
	mov	tx,	ab
	lodsb
	mov	[tx],	a
	ret
	endp
z80_opcode_mov_m_i8:	proc
	;No parameters
	;Covers:
	;066 = $36
	mov	tx,	z80_h	;mov	ab,	[z80_hl]
	mov	b,	[tx]
	mov	tx,	z80_l
	mov	a,	[tx]
	mov	tx,	ab	;mov	tx,	[z80_hl]
	lodsb			;mov	[tx],	[si++]
	mov	[tx],	a
	ret
	endp
z80_opcode_mov_r8_r8:	proc
	;Parameters:
	;C	0ds
	;Pseudo-hyperthreading interleave
	;Split C into C=C[0:2] and D=C[3:5]
	mov	d,	c	;shr	c,	3	;mov	d,	c
	shr	c					
	mov	a,	7				;and	d,	7
	shr	c
	and	d,	a
	shr	c
	nop
	and	c,	a	;and	c,	7
	;--
	mov	ab,	z80_registers;lea di,[z80_registers+c]
	add	a,	c	
	incc	b
	mov	di,	ab
	mov	ab,	z80_registers;lea tx,[z80_registers+d]
	add	a,	d
	incc	b
	mov	tx,	ab
	mov	a,	[tx]	;mov	[di],	[tx]
	mov	[di],	a
	ret
	endp
z80_opcode_mov_r8_m:	proc
	mov	ab,	z80_registers;lea	di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	ldb	z80_h		;mov	tx,	[z80_hl]
	lda	z80_l
	mov	tx,	ab
	mov	a,	[tx]	;mov	[di],	[tx]
	mov	[di],	a
	ret
	endp
z80_opcode_mov_m_r8:	proc
	mov	ab,	z80_registers;lea	di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	ldb	z80_h		;mov	tx,	[z80_hl]
	lda	z80_l
	mov	tx,	ab
	mov	a,	[di]	;mov	[tx](mem),	[di](reg)
	mov	[tx],	a
	ret
	endp
z80_opcode_fast_add_r8:	proc
	mov	ab,	z80_registers;lea di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	add	a,	b
	mov	[tx],	a
	ret
	endp
z80_opcode_fast_sub_r8:	proc
	mov	ab,	z80_registers;lea di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	sub	a,	b
	mov	[tx],	a
	ret
	endp
z80_opcode_add_r8:	proc
	mov	ab,	z80_registers;lea di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	add	a,	b
	mov	[tx],	a	;result into A
	mov	b,	0	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_sub_r8:	proc
	mov	ab,	z80_registers;lea di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	sub	a,	b
	mov	[tx],	a	;result into A
	mov	b,	1	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_adc_r8:	proc
	mov	ab,	z80_registers;lea di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	ldc	z80_f		;mov	c,	[z80_f]
	mov	d,	1	;and	c,	1
	and	c,	d
	;--
	mov	b,	[di]
	mov	a,	[tx]	
	nop
	add	b,	c
	nop
	add	a,	b
	mov	[tx],	a	;result into A
	mov	b,	0	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_sbb_r8:	proc
	mov	ab,	z80_registers;lea di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	b,	[di]
	mov	a,	[tx]
	nop	
	add	b,	c
	nop
	sub	a,	b
	mov	[tx],	a	;result into A
	;--
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	sta	z80_f
	ret		
	endp
z80_opcode_and_r8:	proc
	mov	ab,	z80_registers;lea di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	and	a,	b
	mov	[tx],	a	;result into A
	;--
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	sta	z80_f
	ret			
	endp
z80_opcode_xor_r8:	proc
	mov	ab,	z80_registers;lea di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	xor	a,	b
	mov	[tx],	a	;result into A
	;--
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	sta	z80_f
	ret			
	endp
z80_opcode_or_r8:	proc
	mov	ab,	z80_registers;lea di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	or	a,	b
	mov	[tx],	a	;result into A
	;--
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	sta	z80_f
	ret	
	endp
z80_opcode_cmp_r8:	proc
	mov	ab,	z80_registers;lea di,	[ab+c]
	add	a,	c
	incc	b
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	sub	a,	b
	nop;mov	[tx],	a	;result into A
	mov	b,	1	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_add_m8:	proc
	ldb	z80_h		;mov	di,	ab,	[z80_hl]
	lda	z80_l
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	add	a,	b
	mov	[tx],	a	;result into A
	mov	b,	0	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_sub_m8:	proc
	ldb	z80_h		;mov	di,	ab,	[z80_hl]
	lda	z80_l
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	sub	a,	b
	mov	[tx],	a	;result into A
	mov	b,	1	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_adc_m8:	proc
	ldb	z80_h		;mov	di,	ab,	[z80_hl]
	lda	z80_l
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	ldc	z80_f		;mov	c,	[z80_f]
	mov	d,	1	;and	c,	1
	and	c,	d
	;--
	mov	b,	[di]
	mov	a,	[tx]	
	nop
	add	b,	c
	nop
	add	a,	b
	mov	[tx],	a	;result into A
	mov	b,	0	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_sbb_m8:	proc
	ldb	z80_h		;mov	di,	ab,	[z80_hl]
	lda	z80_l
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	ldc	z80_f		;mov	c,	[z80_f]
	mov	d,	1	;and	c,	1
	and	c,	d
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	sub	a,	b
	nop
	sbb	a,	c
	mov	[tx],	a	;result into A
	mov	b,	1	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_and_m8:	proc
	ldb	z80_h		;mov	di,	ab,	[z80_hl]
	lda	z80_l
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	and	a,	b
	mov	[tx],	a	;result into A
	;--
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	sta	z80_f
	ret			
	endp
z80_opcode_xor_m8:	proc
	ldb	z80_h		;mov	di,	ab,	[z80_hl]
	lda	z80_l
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	xor	a,	b
	mov	[tx],	a	;result into A
	;--
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	sta	z80_f
	ret		
	endp
z80_opcode_or_m8:	proc
	ldb	z80_h		;mov	di,	ab,	[z80_hl]
	lda	z80_l
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	or	a,	b
	mov	[tx],	a	;result into A
	;--
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	sta	z80_f
	ret
	endp
z80_opcode_cmp_m8:	proc
	ldb	z80_h		;mov	di,	ab,	[z80_hl]
	lda	z80_l
	mov	di,	ab
	mov	tx,	z80_a	;mov	tx,	z80_a
	;--
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[di]
	nop
	sub	a,	b
	nop;mov	[tx],	a	;result into A
	mov	b,	1	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_add_i8:	proc
	mov	tx,	z80_a	;mov	tx,	z80_a
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[si]
	inc	si
	add	a,	b
	mov	[tx],	a	;result into A
	mov	b,	0	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_sub_i8:	proc
	mov	tx,	z80_a	
	mov	a,	[tx]	
	mov	b,	[si]
	inc	si
	sub	a,	b
	mov	[tx],	a	;result into A
	mov	b,	1	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_adc_i8:	proc
	mov	tx,	z80_a	;mov	tx,	z80_a
	ldc	z80_f		;mov	c,	[z80_f]
	mov	d,	1	;and	c,	1
	and	c,	d
	;--
	mov	b,	[si]
	mov	a,	[tx]	
	nop
	add	b,	c
	inc	si
	add	a,	b
	mov	[tx],	a	;result into A
	mov	b,	0	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_sbb_i8:	proc
	mov	tx,	z80_a	;mov	tx,	z80_a
	ldc	z80_f		;mov	c,	[z80_f]
	mov	d,	1	;and	c,	1
	and	c,	d
	;--
	mov	b,	[si]
	mov	a,	[tx]	
	inc	si
	add	b,	c
	nop
	sub	a,	b
	mov	[tx],	a	;result into A
	mov	b,	1	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_and_i8:	proc
	mov	tx,	z80_a	;mov	tx,	z80_a
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[si]
	nop
	and	a,	b
	mov	[tx],	a
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	sta	z80_f
	ret			
	endp
z80_opcode_xor_i8:	proc
	mov	tx,	z80_a	;mov	tx,	z80_a
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[si]
	nop
	xor	a,	b
	mov	[tx],	a
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	sta	z80_f
	ret				
	endp
z80_opcode_or_i8:	proc
	mov	tx,	z80_a	;mov	tx,	z80_a
	mov	a,	[tx]	;add	[tx],	[di]
	mov	b,	[si]
	nop
	or	a,	b
	mov	[tx],	a
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	sta	z80_f
	ret			
	endp
z80_opcode_cmp_i8:	proc
	mov	tx,	z80_a	
	mov	a,	[tx]	
	mov	b,	[si]
	inc	si
	sub	a,	b	;result into A
	mov	b,	1	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_fast_inc_r8:	proc
	mov	ab,	z80_registers;lea tx,	[ab+c]
	add	a,	c
	incc	b
	mov	tx,	ab
	mov	a,	[tx]	;inc	[tx]
	inc	a
	mov	[tx],	a	
	ret
	endp
z80_opcode_fast_dec_r8:	proc
	mov	ab,	z80_registers;lea tx,	[ab+c]
	add	a,	c
	incc	b
	mov	tx,	ab
	mov	a,	[tx]	;inc	[tx]
	dec	a
	mov	[tx],	a	
	ret			
	endp
z80_opcode_inc_r8:	proc
	mov	ab,	z80_registers;lea tx,	[ab+c]
	add	a,	c
	incc	b
	mov	tx,	ab
	;--
	mov	a,	[tx]	;inc	[tx]
	inc	a
	mov	[tx],	a	;result into A
	mov	b,	0	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_opcode_dec_r8:	proc
	mov	ab,	z80_registers;lea tx,	[ab+c]
	add	a,	c
	incc	b
	mov	tx,	ab
	;--
	mov	a,	[tx]	;inc	[tx]
	dec	a
	mov	[tx],	a	;result into A
	mov	b,	0	;N flag into B
	jmp	z80_generateflags;Do flags generation
	ret			;return is on flags generation subroutine
	endp
z80_generateflags_lc:	proc
	;Generate flags for shift operations
	;Parameters:
	;A	Result
	mov	b,	0
	jnlc	z80_generateflags_lc.no_c
	mov	b,	1
z80_generateflags_lc.no_c:
	;Generate Sign, Zero and parity from a table
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	add	b,	a
z80_generateflags_lc.no_z:
	stb	z80_f
	ret
	endp
z80_generateflags_p:	proc
	;Parameters:
	;A	Result
	;FL.C	-.- Flags to write to Z80 flags register
	;FL.V	-'
	;Returns:
	;Breaks:
	;All GPRs + FL + TX
	mov	b,	0
	jnc	z80_generateflags_p.no_c
	mov	b,	1	
z80_generateflags_p.no_c:
	mov	th,	z80_signzeroparity/256
	mov	tl,	a
	mov	a,	[tx]
	nop
	add	b,	a
	stb	z80_f
	ret
	endp
z80_generateflags:	proc
	;Parameters:
	;A	Result
	;B	The N flag + 5/3
	;FL.C	-.- Flags to write to Z80 flags register
	;FL.V	-'
	;Returns:
	;Breaks:
	;All GPRs + FL + TX
	;Generate Carry and Overflow from hardware flags
	;Additions are delayed because in "sequential IF switch block"
	;the variable for switch (the flags) must not be changed.
	mov	d,	0
	mov	c,	0
	jnc	z80_generateflags.no_c
	mov	d,	1	
z80_generateflags.no_c:
	jno	z80_generateflags.no_v
	mov	c,	8
z80_generateflags.no_v:	
	add	b,	d
	add	b,	c
	;Generate Sign and Zero
	;Not delaying additions, instead recalculating flags at each check
	test	a
	jns	z80_generateflags.no_s
	mov	d,	0x80
	add	b,	d
z80_generateflags.no_s:
	test	a
	jnz	z80_generateflags.no_z
	mov	d,	0x40
	add	b,	d
z80_generateflags.no_z:
	stb	z80_f
	;s z 0 0 0 v n c
	ret
	endp
z80_opcode_call_a16:	proc
	lda	z80_sp		;mov	di,	[z80_sp];setup stack pointer
	ldb	z80_sp+1
	mov	di,	ab
	lodsb			;lodsw	ab,	[si++]	;fetch address for call
	mov	b,	[si]
	inc	si
	nop
	mov	cd,	si	;mov	[--di],	si	;store return address
	dec	di		
	mov	[di],	d
	dec	di
	mov	[di],	c
	mov	si,	ab	;switch pc to the fetched address
	mov	ab,	di	;mov	[z80_sp],di	;store changed sp
	sta	z80_sp
	stb	z80_sp+1
	ret
	endp
z80_opcode_ret:		proc
	lda	z80_sp		;mov	di,	[z80_sp];setup stack pointer
	ldb	z80_sp+1
	mov	di,	ab
	mov	a,	[di]	;mov	si,	[di++]	;fetch return address
	inc	di					;and jump there
	mov	b,	[di]
	inc	di
	mov	si,	ab
	mov	ab,	di	;mov	[z80_sp],di	;store changed sp
	sta	z80_sp
	stb	z80_sp+1
	ret
	endp
z80_opcode_push_rp:	proc
	;As the argument, takes ID of reg. pair, multiplied by 2.
	;That is,
	;	BC	0 
	;	DE	2 
	;	HL	4 
	;	AF	6 
	;	BC'	8 
	;	DE'	10
	;	HL'	12
	;	AF'	14
	;	IX	16
	;	IY	18
	;	SP	20
	;	PC	not there
	;This simplifies register EA calculation, and we do not need to bother
	;with making such a value - we are taking it from a table anyways.
	;Note that because of register ordering, in AF regpair F is high byte
	;and A is lower byte.
	push	si
	mov	ab,	z80_registers;lea di,	ab,	[z80_registers+c]
	add	a,	c	
	incc	b
	nop
	mov	di,	ab	
	lda	z80_sp		;mov	si,	ab,	[z80_sp]
	ldb	z80_sp+1	;optimizable
	mov	si,	ab
	dec	si		;mov	word	[--si],	[di]
	mov	a,	[di]	
	mov	[si],	a	
	inc	di
	dec	si
	mov	a,	[di]	
	mov	[si],	a	
	nop
	mov	ab,	si	;mov	[z80_sp],	ab,	si
	sta	z80_sp
	stb	z80_sp+1
	pop	si
	ret
	endp
z80_opcode_pop_rp:	proc
	push	si
	mov	ab,	z80_registers;lea di,	ab,	[z80_registers+c]
	add	a,	c	
	incc	b
	mov	di,	ab	
	lda	z80_sp		;mov	si,	ab,	[z80_sp]
	ldb	z80_sp+1	;optimizable
	mov	si,	ab
	inc	di		;mov	[di+1],	[si++]
	mov	a,	[si]
	mov	[di],	a
	dec	di
	inc	si		
	mov	a,	[si]	;mov	[di],	[si++]
	mov	[di],	a
	inc	si
	mov	ab,	si	;mov	[z80_sp],	ab,	si
	sta	z80_sp
	stb	z80_sp+1
	pop	si
	ret
	endp
z80_opcode_jmp_a16:	proc
	;Absolute jump
	lodsb			;lodsw			;fetch address for jump
	mov	b,	[si]
	inc	si
	mov	si,	ab				;set PC to the address
	ret
	endp
z80_opcode_ex_de_hl:	proc 
	mov	di,	z80_d	
	mov	a,	[di]	;mov	ab,	[di]
	inc	di
	mov	b,	[di]
	ldc	z80_h		;mov	cd,	[z80_hl]
	ldd	z80_l
	sta	z80_h		;mov	[z80_hl],	ab
	stb	z80_l
	mov	[di],	d	;mov	[z80_de],	cd
	dec	di
	mov	[di],	c
	ret
	endp
z80_opcode_ex_de_ix:	proc
	lda	z80_e
	ldb	z80_d
	ldc	z80_ixl		
	ldd	z80_ixh
	sta	z80_ixl		
	stb	z80_ixh
	stc	z80_e
	stc	z80_d
	ret
	endp
z80_opcode_ex_de_iy:	proc
	lda	z80_e
	ldb	z80_d
	ldc	z80_iyl		
	ldd	z80_iyh
	sta	z80_iyl		
	stb	z80_iyh
	stc	z80_e
	stc	z80_d
	ret
	endp
z80_opcode_ex_af_af:	proc
	mov	di,	z80_f
	mov	a,	[di]	;mov	ab,	[di]
	inc	di
	mov	b,	[di]
	ldc	z80_f2		;mov	cd,	[z80_hl]
	ldd	z80_a2
	sta	z80_f2		;mov	[z80_hl],	ab
	stb	z80_a2
	mov	[di],	d	;mov	[z80_de],	cd
	dec	di
	mov	[di],	c
	ret
	endp
z80_opcode_exx:		proc
	;Note that EXX does not exchanges AF and AF'.
	push	si
	mov	si,	z80_b
	mov	di,	z80_b2
	mov	c,	6
z80_opcode_exx.loop:	
	mov	a,	[si]
	mov	b,	[di]
	mov	[di],	a
	mov	[si],	b
	inc	si
	inc	di
	dec	c
	jnz	z80_opcode_exx.loop
	pop	si
	ret
	endp
z80_opcode_mov_r16_i16:	proc
	mov	ab,	z80_registers;lea di,	ab,	[z80_registers+c]
	add	a,	c	
	incc	b
	mov	di,	ab	
	lodsw			;mov	[di],	ab,	[si]
	mov	b,	[di]	
	inc	di
	mov	a,	[di]
	ret
	endp
z80_opcode_mov_littleendian_r16_i16:	proc
	mov	ab,	z80_registers;lea di,	ab,	[z80_registers+c]
	add	a,	c	
	incc	b
	mov	di,	ab	
	lodsw			;mov	[di],	ab,	[si]
	mov	a,	[di]	
	inc	di
	mov	b,	[di]
	ret
	endp
z80_opcode_ncall_a16:	proc
	;Do a subroutine call to a native subroutine.
	push	ra
	push	si	;SI needs to be preserved - native subrouties are likely 
			;to use it.
	lodsb			;mov	ab,	[si++]
	mov	b,	[si]
	inc	si
	mov	di,	ab	
	call	di		;The native call to a native subroutine
	pop	si	;Restoration of SI - native subroutine likely destroyed it
	pop	ra
	ret
	endp
z80_opcode_ncall_hl:	proc
	push	ra
	push	si	;SI needs to be preserved - native subrouties are likely 
			;to use it.
	lda	z80_l
	ldb	z80_h
	mov	di,	ab	
	call	di		;The native call to a native subroutine
	pop	si	;Restoration of SI - native subroutine likely destroyed it
	pop	ra
	ret
	endp
z80_opcode_jmp_cc_a16:	proc
	clc			;lea	di,	[ab+z80_condcodes_to_flagmask*2]
	shl	c
	mov	ab,	z80_condcodes_to_flagmask
	add	a,	c
	incc	b
	mov	di,	ab
	lodsb			;mov	ab,	[si++]
	mov	b,	[si]
	inc	si
	ldc	z80_f		;mov	c,	[z80_f]
	mov	d,	[di]	;xor	c,	[di]
	inc	di		;and	c,	[di+1]
	xor	c,	d
	mov	d,	[di]
	inc	di
	and	c,	d
	jnz	z80_opcode_jmp_cc_a16.ret
	mov	si,	ab
z80_opcode_jmp_cc_a16.ret:
	ret
	endp
z80_opcode_call_cc_a16:	proc
	clc			;lea	di,	[z80_condcodes_to_flagmask+c*2]
	shl	c
	mov	ab,	z80_condcodes_to_flagmask
	add	a,	c
	incc	b
	mov	di,	ab
	lodsb			;mov	ab,	[si++]
	mov	b,	[si]
	inc	si
	ldc	z80_f		;mov	c,	[z80_f]
	mov	d,	[di]	;xor	c,	[di]
	inc	di		;and	c,	[di+1]
	xor	c,	d
	mov	d,	[di]
	inc	di
	and	c,	d
	jnz	z80_opcode_call_cc_a16.ret
	push	ab		;mov	di,	[z80_sp];setup stack pointer
	lda	z80_sp		
	ldb	z80_sp+1
	mov	di,	ab
	pop	ab
	mov	cd,	si	;mov	[--di],	si	;store return address
	dec	di		
	mov	[di],	d
	dec	di
	mov	[di],	c
	mov	si,	ab	;switch pc to the fetched address
	mov	ab,	di	;mov	[z80_sp],di	;store changed sp
	sta	z80_sp
	stb	z80_sp+1
z80_opcode_call_cc_a16.ret:
	ret
	endp

z80_opcode_ret_cc_a16:	proc
	clc			;lea	di,	[ab+z80_condcodes_to_flagmask*2]
	shl	c
	mov	ab,	z80_condcodes_to_flagmask
	add	a,	c
	incc	b
	mov	di,	ab
	ldc	z80_f		;mov	c,	[z80_f]
	mov	d,	[di]	;xor	c,	[di]
	inc	di		;and	c,	[di+1]
	xor	c,	d
	mov	d,	[di]
	inc	di
	and	c,	d
	jnz	z80_opcode_ret_cc_a16.ret
	mov	si,	ab
z80_opcode_ret_cc_a16.ret:
	lda	z80_sp		;mov	di,	[z80_sp];setup stack pointer
	ldb	z80_sp+1
	mov	di,	ab
	mov	a,	[di]	;mov	si,	[di++]	;fetch return address
	inc	di					;and jump there
	mov	b,	[di]
	inc	di
	mov	si,	ab
	mov	ab,	di	;mov	[z80_sp],di	;store changed sp
	sta	z80_sp
	stb	z80_sp+1
	ret
	endp
z80_opcode_rst_nn:	proc
	;May be used to do an NMI w/external IFF flipping
	lda	z80_sp		;mov	di,	[z80_sp];setup stack pointer
	ldb	z80_sp+1
	mov	di,	ab
	mov	ab,	si	;mov	[--di],	ab,	si	;store return address
	dec	di		
	mov	[di],	b
	dec	di
	mov	[di],	a
	mov	th,	0	;switch pc to the restart address
	mov	tl,	c
	nop
	mov	si,	tx
	mov	ab,	di	;mov	[z80_sp],di	;store changed sp
	sta	z80_sp
	stb	z80_sp+1
	ret
	endp
z80_opcode_jr_a8:	proc
	mov	c,	[si]	;mov	c,	[si++]
	inc	si
	nop
	mov	b,	c	;movsx	cd,	c
	add	b,	b
	mov	b,	0
	mov	d,	0
	sbb	d,	b
	mov	ab,	si	;mov	ab,	si	;add	si,	cd
	add	a,	c	;adc	ab,	cd	;do the relative jump
	adc	b,	d
	nop
	mov	si,	ab	;mov	si,	ab
	endp
z80_opcode_jr_cc_a8:	proc
	clc			;lea	di,	[ab+z80_condcodes_to_flagmask*2]
	shl	c
	mov	ab,	z80_condcodes_to_flagmask
	add	a,	c
	incc	b
	mov	di,	ab
	lodsb			;mov	a,	[si++]
	ldc	z80_f		;mov	c,	[z80_f]
	mov	d,	[di]	;xor	c,	[di]
	inc	di		;and	c,	[di+1]
	xor	c,	d
	mov	d,	[di]
	inc	di
	and	c,	d
	jnz	z80_opcode_jr_cc_a8.ret
	mov	b,	a	;movsx	cd,	a
	mov	c,	a
	add	b,	b
	mov	b,	0
	mov	d,	0
	sbb	d,	b
	mov	ab,	si	;mov	ab,	si	;add	si,	cd
	add	a,	c	;adc	ab,	cd	;do the relative jump
	adc	b,	d
	nop
	mov	si,	ab	;mov	si,	ab
z80_opcode_jr_cc_a8.ret:
	ret
	endp
z80_opcode_mov_sp_hl:	proc
	lda	z80_sp
	ldb	z80_sp+1
	sta	z80_l
	stb	z80_h
	ret
	endp
z80_opcode_debug:	proc
	;Debug opcode, displays internal state.
	;Speed of execution is not relevant - it interfaces with SIO anyways.
	push	ra
	push	si
	mov	ab,	si
	mov	cd,	si
	;---
	mov	si,	msg_debugopcode
	call	uart_write
	;---
	call	uart_write
	mov	di,	si
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	foldstart
	call	uart_write
	mov	di,	si
	ldb	z80_a
	lda	z80_f
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	mov	di,	si
	ldb	z80_b
	lda	z80_c
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	mov	di,	si
	ldb	z80_d
	lda	z80_e
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	mov	di,	si
	ldb	z80_h
	lda	z80_l
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	mov	di,	si
	ldb	z80_ixl
	lda	z80_ixh
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	mov	di,	si
	ldb	z80_iyl
	lda	z80_iyh
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	mov	di,	si
	lda	z80_sp
	ldb	z80_sp+1
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	push	si
	mov	di,	cd
	dec	di
	mov	b,	[di]
	inc	di
	mov	a,	[di]
	call	math_itoahex_16
	call	uart_write
	pop	si
	;---
	call	uart_write
	mov	di,	si
	ldb	z80_a2
	lda	z80_f2
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	mov	di,	si
	ldb	z80_b2
	lda	z80_c2
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	mov	di,	si
	ldb	z80_d2
	lda	z80_e2
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	mov	di,	si
	ldb	z80_h2
	lda	z80_l2
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	mov	di,	si
	ldb	z80_i
	lda	z80_r
	call	math_itoahex_16
	call	uart_write
	mov	si,	di
	;---
	call	uart_write
	mov	di,	si
	lda	z80_iff
	call	math_itoahex_8
	call	uart_write
	mov	si,	di
	call	uart_write
	foldend
	;The flags
	ldb	z80_f
	mov	di,	si
	foldstart
	inc	di
	inc	di
	add	b,b
	incc	si
	mov	a,	[si]
	call	Uart_Write_Char
	mov	si,	di
	mov	a,	32
	call	uart_write_char
	;--
	inc	di
	inc	di
	add	b,b
	incc	si
	mov	a,	[si]
	call	Uart_Write_Char
	mov	si,	di
	mov	a,	32
	call	uart_write_char
	;--
	inc	di
	inc	di
	add	b,b
	incc	si
	mov	a,	[si]
	call	Uart_Write_Char
	mov	si,	di
	mov	a,	32
	call	uart_write_char
	;--
	inc	di
	inc	di
	add	b,b
	incc	si
	mov	a,	[si]
	call	Uart_Write_Char
	mov	si,	di
	mov	a,	32
	call	uart_write_char
	;--
	inc	di
	inc	di
	add	b,b
	incc	si
	mov	a,	[si]
	call	Uart_Write_Char
	mov	si,	di
	mov	a,	32
	call	uart_write_char
	;--
	inc	di
	inc	di
	add	b,b
	incc	si
	mov	a,	[si]
	call	Uart_Write_Char
	mov	si,	di
	mov	a,	32
	call	uart_write_char
	;--
	inc	di
	inc	di
	add	b,b
	incc	si
	mov	a,	[si]
	call	Uart_Write_Char
	mov	si,	di
	mov	a,	32
	call	uart_write_char
	;--
	inc	di
	inc	di
	add	b,b
	incc	si
	mov	a,	[si]
	call	Uart_Write_Char
	mov	si,	di
	mov	a,	32
	call	uart_write_char
	foldend
	call	uart_write
	;==
	mov	di,	0
 z80_opcode_debug.delayloop:
	dec	di
	mov	ab,	di
	nop
	or	a,	b
	jnz	z80_opcode_debug.delayloop
	pop	si
	pop	ra
	ret
	endp
z80_opcode_endsession:	proc
	;Handler for correct termination of emulator.
	nop
	jmp	0xE000
	push	ra
	push	si
	mov	si,	msg_sessionend
	call	uart_write
	pop	si
	pop	ra
	ret
	endp
z80_opcode_undefined:	proc
	push	si			
	mov	si,	msg_undefined_opcode
	call	uart_write
	pop	si
	call	z80_opcode_debug
	;jmp	os_terminate	;In case of an unimplemented opcode, we should 
	;terminate, as the program excepts us to do something yet we cant,
	;while if an opcode is undefined, we are fine - program does not excepts
	;us to do anything anyways either, so we may just throw a warning and
	;return to the main loop.
	ret
	endp
z80_opcode_unimplemented:	proc
	;jmp	0xE000
	push	si			
	mov	si,	msg_unimplemented_opcode
	call	uart_write
	pop	si
	call	z80_opcode_debug
	jmp	os_terminate
	ret
	endp
native_debug:	proc
	push	tx
	push	ra
	push	di
	push	si
	push	a
	push	b
	push	c
	push	d
	mov	tx,	sp	;lea	si,	[sp]
	mov	di,	tx	;a stack frame! 1st in the program!
	;inc	di
	;---
	mov	si,	msg_nativedebug
	call	uart_write
	;--
	call	uart_write
	push	si
	mov	tx,	ra
	mov	ab,	tx
	call	math_itoahex_16
	call	uart_write
	pop	si
	;--
	call	uart_write
	push	si
	mov	tx,	sp
	mov	ab,	tx
	call	math_itoahex_16
	call	uart_write
	pop	si
	;--
	mov	c,	6
	push	di
native_debug.loop:
	mov	b,	[di]
	inc	di
	mov	a,	[di]
	inc	di
	call	uart_write
	push	si
	call	math_itoahex_16
	call	uart_write
	pop	si
	dec	c
	jnz	native_debug.loop
	pop	di
	;---
	call	uart_write
	;---
	pop	d
	pop	c
	pop	b
	pop	a
	pop	si
	pop	di
	pop	ra
	pop	tx
	ret
	endp

os_terminate:	proc
	mov	si,	msg_terminated
	call	uart_write
	jmp	0xE000
	ret
	endp
.include lib\Vga.inc
.include lib\Uart.inc
.include lib\Math.inc
end: 
.segment EndOfRom
TopRomAddress:
;