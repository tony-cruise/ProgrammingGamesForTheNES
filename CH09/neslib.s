;*****************************************************************
; neslib.s: NES Function Library
;*****************************************************************

; Define PPU Registers
PPU_CONTROL_1 = $2000 ; PPU Control Register 1 (Write)
PPU_CONTROL_2 = $2001 ; PPU Control Register 2 (Write)
PPU_STATUS = $2002; PPU Status Register (Read)
PPU_SPRRAM_ADDRESS = $2003 ; PPU SPR-RAM Address Register (Write)
PPU_SPRRAM_IO = $2004 ; PPU SPR-RAM I/O Register (Write)
PPU_VRAM_ADDRESS1 = $2005 ; PPU VRAM Address Register 1 (Write)
PPU_VRAM_ADDRESS2 = $2006 ; PPU VRAM Address Register 2 (Write)
PPU_VRAM_IO = $2007 ; VRAM I/O Register (Read/Write)
SPRITE_DMA = $4014 ; Sprite DMA Register

; Define APU Registers
APU_DM_CONTROL = $4010 ; APU Delta Modulation Control Register (Write)
APU_CLOCK = $4015 ; APU Sound/Vertical Clock Signal Register (Read/Write)

; Joystick/Controller values
JOYPAD1 = $4016 ; Joypad 1 (Read/Write)
JOYPAD2 = $4017 ; Joypad 2 (Read/Write)

; Gamepad bit values
PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80

; Useful PPU memory addresses
NAME_TABLE_0_ADDRESS		= $2000
ATTRIBUTE_TABLE_0_ADDRESS	= $23C0
NAME_TABLE_1_ADDRESS		= $2400
ATTRIBUTE_TABLE_1_ADDRESS	= $27C0

.segment "ZEROPAGE"

nmi_ready:		.res 1 ; set to 1 to push a PPU frame update, 
					   ;        2 to turn rendering off next NMI

.include "macros.s"

;*****************************************************************
; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
;*****************************************************************
.segment "CODE"
.proc ppu_update
	lda #1
	sta nmi_ready
	loop:
		lda nmi_ready
		bne loop
	rts
.endproc

;*****************************************************************
; ppu_off: waits until next NMI, turns rendering off (now safe to write PPU directly via PPU_VRAM_IO)
;*****************************************************************
.segment "CODE"
.proc ppu_off
	lda #2
	sta nmi_ready
	loop:
		lda nmi_ready
		bne loop
	rts
.endproc

;*****************************************************************
; clear_nametable: clears the first name table
;*****************************************************************
.segment "CODE"
.proc clear_nametable
 	lda PPU_STATUS ; reset address latch
 	lda #$20 ; set PPU address to $2000
 	sta PPU_VRAM_ADDRESS2
 	lda #$00
 	sta PPU_VRAM_ADDRESS2

 	; empty nametable
 	lda #0
 	ldy #30 ; clear 30 rows
 	rowloop:
 		ldx #32 ; 32 columns
 		columnloop:
 			sta PPU_VRAM_IO
 			dex
 			bne columnloop
 		dey
 		bne rowloop

 	; empty attribute table
 	ldx #64 ; attribute table is 64 bytes
 	loop:
 		sta PPU_VRAM_IO
 		dex
 		bne loop
 	rts
 .endproc

.segment "ZEROPAGE"

gamepad:		.res 1 ; stores the current gamepad values

;*****************************************************************
; gamepad_poll: this reads the gamepad state into the variable labelled "gamepad"
; This only reads the first gamepad, and also if DPCM samples are played they can
; conflict with gamepad reading, which may give incorrect results.
;*****************************************************************
.segment "CODE"
.proc gamepad_poll
	; strobe the gamepad to latch current button state
	lda #1
	sta JOYPAD1
	lda #0
	sta JOYPAD1
	; read 8 bytes from the interface at $4016
	ldx #8
loop:
	pha
	lda JOYPAD1
	; combine low two bits and store in carry bit
	and #%00000011
	cmp #%00000001
	pla
	; rotate carry into gamepad variable
	ror a
	dex
	bne loop
	sta gamepad
	rts
.endproc

;*****************************************************************
; write_text: This writes a section of text to the screen
; text_address - points to the text to write to the screen
; PPU address has been set
;*****************************************************************

.segment "ZEROPAGE"

text_address:	.res 2 ; set to the address of the text to write

.segment "CODE"
.proc write_text
	ldy #0
loop:
	lda (text_address),y ; get the byte at the current source address
	beq exit ; exit when we encounter a zero in the text
	sta PPU_VRAM_IO ; write the byte to video memory
	iny
	jmp loop
exit:
	rts
.endproc

;*****************************************************************
; randomize: Get a random value from the current SEED values
;*****************************************************************

.segment "ZEROPAGE"

SEED0: .res 2
SEED2: .res 2

; simple shift based random number
.segment "CODE"
.proc randomize
	lda SEED0
	lsr
	rol SEED0+1
	bcc @noeor
	eor #$B4
@noeor:
	sta SEED0
	eor SEED0+1
	rts
.endproc

; Linear Frequency random numbers
; result in a (lo) and y (hi)
.proc rand
	jsr rand64k	; Factors of 65536: 3 5 17 257
	jsr rand32k ; Factors of 32767; 7 31 151
	lda SEED0+1	; combine other seed values
	eor SEED2+1
	tay	; save hi byte
	lda SEED0	; mix up lowbytes of SEED0
	eor SEED2	; and SEED2 to combine both
	rts
.endproc

.proc rand64k
	lda SEED0+1
	asl
	asl
	eor SEED0+1
	asl
	eor SEED0+1
	asl
	asl
	eor SEED0+1
	asl
	rol SEED0	; shift this left, "random" bit comes from low
	rol SEED0+1
	rts
.endproc

.proc rand32k
	lda SEED2+1
	asl
	eor SEED2+1
	asl
	asl
	ror SEED2	; shift this right, random bit comes from high
	rol SEED2+1
	rts
.endproc

;*****************************************************************
; collision_test: Check whether two objects have hit each other
; Returns: Carry flag set if objects have hit each other
;*****************************************************************

.segment "ZEROPAGE"

cx1:	.res 1 ; object 1 X position
cy1:	.res 1 ; object 1 Y position
cw1:	.res 1 ; object 1 width
ch1:	.res 1 ; object 1 height

cx2:	.res 1 ; object 2 X position
cy2:	.res 1 ; object 2 Y position
cw2:	.res 1 ; object 2 width
ch2:	.res 1 ; object 2 height

.segment "CODE"

.proc collision_test
	clc
	lda cx1 ; get object 1 x
	adc cw1 ; add object 1 width
	cmp cx2 ; is object 2 to the right of object 1 plus it's width?
	bcc @exit
	clc
	lda cx2 ; get object 2 x
	adc cw2 ; add object 2 width
	cmp cx1 ; is object 2 to the left of object 1?
	bcc @exit
	lda cy1 ; get object 1 y
	adc ch1 ; add object 1 height
	cmp cy2 ; is object 2 below object 1 plus it's height?
	bcc @exit
	clc
	lda cy2 ; get object 2 y
	adc ch2 ; add object 2 height
	cmp cy1 ; is object 2 above object 1?
	bcc @exit

	sec ; we have hit, set carry flag and exit
	rts
@exit:
	clc ; clear carry flag and exit
	rts
.endproc



