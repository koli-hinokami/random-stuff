 eflags.cf	= 0x1
;eflags.kf	= 0x2		;SF ^ L.OF
 eflags.pf	= 0x4
;eflags.und	= 0x8
 eflags.af	= 0x10
;eflags.legacyoverflow	= 0x20
 eflags.zf	= 0x40
 eflags.sf	= 0x80
 eflags.tf	= 0x100
 eflags.if	= 0x200
 eflags.df	= 0x400
 eflags.of	= 0x800
 eflags.iopl	= 0x1000
;eflags.iopl	= 0x2000
 eflags.nt	= 0x4000
;eflags.0	= 0x8000
 eflags.vm	= 0x10000
 


    virtual at 0
	intvectable	rw	256*2 ;0:0
    end virtual
    virtual at 0x300
	bootstack	rw	128   ;30:0
	    .top:	
	biosdata:
	    biosdata.portaddr:
	    biosdata.portaddr.com1	rw	1
	    biosdata.portaddr.com2	rw	1
	    biosdata.portaddr.com3	rw	1
	    biosdata.portaddr.com4	rw	1
	    biosdata.portaddr.lpt1	rw	1
	    biosdata.portaddr.lpt2	rw	1
	    biosdata.portaddr.lpt3	rw	1
	    biosdata.ext_bda_seg	rw	1
	    biosdata.equipment_list_fl	rw	1
	    biosdata.pcjr_ir_kbd_errs	rb	1
	    biosdata.memsize_kb		rw	1
					rb 1
	    biosdata.ps2_bios_flags	rb	1
	    biosdata.keyboard:
	    biosdata.keyboard.flags0	rb	1
	    biosdata.keyboard.flags1	rb	1
	    biosdata.keyboard.alt_kp_entry 	rb	1
	    biosdata.keyboard.kbd_buff_head	rw	1
	    biosdata.keyboard.kbd_buff_tale	rw	1
	    biosdata.keyboard.kbd_buff	  	rb	32
	    biosdata.fdd:
	    biosdata.fdd.drive_recalib		rb	1
	    biosdata.fdd.motor_status 		rb	1
	    biosdata.fdd.motor_shutoff_cnt 	rb	1
	    biosdata.fdd.last_cmd_status 	rb	1
	    biosdata.fdd.fdc_status 		rb	7
	    biosdata.video:
	    biosdata.video.curvideomode		rb	1
	    biosdata.video.screencolumns	rw	1
	    biosdata.video.video_regen_buffer_size	rw	1
	    biosdata.video.video_regen_buff_cur_page_offset	rw	1
	    biosdata.video.cursor_pos:
	    biosdata.video.cursor_pos.page1		rw	1
	    biosdata.video.cursor_pos.page2		rw	1
	    biosdata.video.cursor_pos.page3		rw	1
	    biosdata.video.cursor_pos.page4		rw	1
	    biosdata.video.cursor_pos.page5		rw	1
	    biosdata.video.cursor_pos.page6		rw	1
	    biosdata.video.cursor_pos.page7		rw	1
	    biosdata.video.cursor_pos.page8		rw	1
	    biosdata.video.cur_ending_scanline		rb	1  ;	- related to retrace, dont modify
	    biosdata.video.cur_starting_scanline	rb	1;	/
	    biosdata.video.activepage			rb	1
	    biosdata.crtc:
	    biosdata.crtc.baseport			rw	1
	    biosdata.crtc.mode_control_reg_val		rb	1
	    biosdata.video.cgacurpalette		rb	1
	    biosdata.sssp_temp_storage			rd	1
	    biosdata.clock:
	    biosdata.clock.daily_timer_cnt		rd	1
	    biosdata.clock.clock_rollover_flag		rb	1
	    biosdata.bios_break_flag			rb	1
	    biosdata.soft_reset_flag			rw	1
	    biosdata.hdd:
	    biosdata.hdd.lastcmd	rb	1
	    biosdata.hdd.hdcount	rb	1
	    biosdata.hdd.xtcontrolbyte	rb	1
	    biosdata.hdd.portoffset	rb	1
	    biosdata.porttimeout:
	    biosdata.porttimeout.lpt1	rb	1
	    biosdata.porttimeout.lpt2	rb	1
	    biosdata.porttimeout.lpt3	rb	1
	    biosdata.porttimeout.lpt4	rb	1
	    biosdata.porttimeout.com1	rb	1
	    biosdata.porttimeout.com2	rb	1
	    biosdata.porttimeout.com3	rb	1
	    biosdata.porttimeout.com4	rb	1
	    biosdata.kdbbuffstartoffset	rw 	1  ; -	BIOS 10-27-82
	    biosdata.kdbbuffendoffset	rw 	1  ; /
	    biosdata.ega:
	    biosdata.ega.rowsonscreen		rb	1
	    biosdata.ega.charmatrixpointheight	rw	1
	    biosdata.ega.videomodeoptions	rb	1
	    biosdata.ega.featureswitches	rb	1
	    biosdata.ega.videodisplaydataarea	rb	1
	    biosdata.ega.dcc_table_index	rb	1
	    biosdata.fdd.last_datarate		rb	1
	    biosdata.hdd.status			rb	1
	    biosdata.hdd.error			rb	1
	    biosdata.hdd.intcontrolflag		rb	1
	    biosdata.combhardfloppydiskcard	rb	1
	    biosdata.fdd.drivemediastate	rb	4
	    biosdata.fdd.curtrack 		rb	2
	    biosdata.kbd.modetype 		rb	1
	    biosdata.kbd.ledflags 		rb	1
	    biosdata.userwaitcompleteflagptr 	rd	1
	    biosdata.userwaittimeoutval		rd	1
	    biosdata.rtcwaitfuncflag 		rb	1
	    biosdata.lana_dma_chan_flags	rb	1
	    biosdata.lana_status 		rb	2
	    biosdata.hdd.saved_intvec 		 rd	1
	    biosdata.biosvideosaveoverrideptrtbl rd	1;?
						rb	8
	    biosdata.kbd.nmicontrolflags 	rb	1
	    biosdata.kbd.brkpendingflags 	rd	1
	    biosdata.port60buff			rb	1
	    biosdata.kbd.lastscancode 		rb	1
	    biosdata.kbd.nmi.buffheadptr 	rb	1
	    biosdata.kbd.nmi.bufftailptr 	rb	1
	    biosdata.kbd.nmi.scancodebuff	rb	16
	    biosdata.daycnt2 			rw	1;convertible and after
	    biosdata.intraappcommunicationsarea	rb	16
	    ;50:00
	dosdata:
	    dosdata.printscreen.statusbyte	rb	1
						rb	3
	    dosdata.dos.singledisketteflag	rb	1
	    dosdata.post.workarea		rb	10
	    dosdata.basic.shellflag		rb	1
	    dosdata.basic.def_ds		rw	1
	    dosdata.basic.int1c			rd	1
	    dosdata.basic.int23			rd	1
	    dosdata.basic.int24			rd	1
	    dosdata.dos.dynstorage		rw	1
	    dosdata.dos.disketteinittbl		rb	14
	    ;50:30
	    dosdata.modecmd 			rb	4
	    rb 12
    end	virtual
