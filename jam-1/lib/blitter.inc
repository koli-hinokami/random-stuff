	.segment Libcode
blitter_copy:	proc
	push	ra
	push	si
	push	di
	push	ab
	push	cd
_init:	;Initialize positions
	lda	blitter_src_x1
	sta	_sx
	lda	blitter_src_y1
	sta	_sy
	lda	blitter_dst_x1
	sta	_dx
	lda	blitter_dst_y1
	sta	_dy
_loopy:
	
_loopx:
	lxa	blitter_srcfetch
	mov	di,	ab
	lxa	blitter_src
	mov	si,	ab
	lda	_sx
	ldb	_sy
	ldd	blitter_src_xs
	call	di
	lxa	blitter_dstwrite
	mov	di,	ab
	lxa	blitter_dst
	mov	si,	ab
	lda	_dx
	ldb	_dy
	ldd	blitter_dst_xs
	call	di
	
_lex:	
	inc_a_abs	_dx		
	inc_a_abs	_sx
	cmp_b_abs	a,	blitter_src_x2
	jbe	_loopx
	lda	blitter_src_x1
	sta	_sx
	lda	blitter_dst_x1
	sta	_dx
_ley:	inc_a_abs	_dy		
	inc_a_abs	_sy		
	cmp_b_abs	a,	blitter_src_y2
	jbe	_loopy
_ret:	pop	cd
	pop	ab
	pop	di
	pop	si
	pop	ra
	ret
_sx:	db 0
_sy:	db 0
_dx:	db 0
_dy:	db 0

	.subsegment
	endp
blitter_fetch_stub:	proc	;void blitter_fetch_stub (__reg_si__ void* bitmap, __reg_a__ xcoord, __reg_b__ ycoord, __reg_d__ xsize);
	push	ra
	push	si
	push	ab
	push	c
	mov	si,	_str
	call	uart_writestring
	call	Math_ITOA_8_Pad3
	call	uart_writestring
	mov	si,	_sepr
	call	uart_writestring
	mov	a,	b
	call	Math_ITOA_8_Pad3
	call	uart_writestring
	mov	si,	_sepr
	call	uart_writestring
	mov	a,	d
	call	Math_ITOA_8_Pad3
	call	uart_writestring
	call	Uart_NewLine
	pop	c
	pop	ab
	pop	si
	pop	ra
	ret
_str:	db	"Blitter read stub:  ",0
_sepr:	db	", ",0
	.subsegment
	endp
blitter_write_stub:	proc	;void blitter_fetch_stub (__reg_si__ void* bitmap, __reg_a__ xcoord, __reg_b__ ycoord, __reg_d__ xsize);
	push	ra
	push	si
	push	ab
	push	c
	mov	si,	_str
	call	uart_writestring
	call	Math_ITOA_8_Pad3
	call	uart_writestring
	mov	si,	_sepr
	call	uart_writestring
	mov	a,	b
	call	Math_ITOA_8_Pad3
	call	uart_writestring
	mov	si,	_sepr
	call	uart_writestring
	mov	a,	d
	call	Math_ITOA_8_Pad3
	call	uart_writestring
	mov	si,	_sepr
	call	uart_writestring
	mov	a,	c
	call	Math_ITOA_8_Pad3
	call	uart_writestring
	call	Uart_NewLine
	pop	c
	pop	ab
	pop	si
	pop	ra
	ret
_str:	db	"Blitter write stub: ",0
_sepr:	db	", ",0
	.subsegment
	endp
blitter_fetch_8bpp:	proc
	push	ra
	push	a		;save X offset
	 mov	a,	d	;get Y pos and X size into AB
	 call	math_multiply_8_8;and multiply into CD
	pop	a		;get X offset back
	addzx	cd,	a	;sum up to get offset
	mov	ab,	si	;add bitmap base to bitmap offset
	add	ab,	cd
	mov	c,	[ab]	;fetch data
	pop	ra
	ret
	endp
	.segment DataRW
;			;Source
blitter_src:	dw 0	;Bitmap
blitter_srcpal: dw 0	;Palette
blitter_srctiles: dw 0	;Tiledata
blitter_srcfetch: dw blitter_fetch_stub;blitter_fetch_stub	;Bitmap fetch: 
;__reg_d__ void (*blitter_srcfetch)(__reg_si__ void* bitmap, __reg_a__ xcoord, __reg_b__ ycoord, __reg_d__ xsize);
blitter_src_xs:	dw 0	;Bitmap x-size
blitter_src_x1:	dw 0	;Start x
blitter_src_y1:	dw 0	;Start y
blitter_src_x2:	dw 0	;End x
blitter_src_y2:	dw 0	;End y
;			;Destination
blitter_dst:	dw 0	;Bitmap
blitter_dstwrite: dw blitter_write_stub ;Bitmap write:
;void (*blitter_dstwrite)(__reg_si__ void* bitmap, __reg_a__ xcoord, __reg_b__ ycoord, __reg_d__ xsize, __reg_c__ color);
blitter_dst_xs:	dw 0	;Bitmap x-size
blitter_dst_x1:	dw 0
blitter_dst_y1:	dw 0
