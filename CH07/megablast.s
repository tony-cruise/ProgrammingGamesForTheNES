; Programming Games for NES - Megablast

;*****************************************************************
; Define NES cartridge Header
;*****************************************************************

.segment "HEADER"
INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 0 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID 
.byte $02 ; 16k PRG bank count
.byte $01 ; 8k CHR bank count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

;*****************************************************************
; Import both the background and sprite character sets
;*****************************************************************

.segment "TILES"
.incbin "megablast.chr"

;*****************************************************************
; Define NES interrupt vectors
;*****************************************************************

.segment "VECTORS"
.word nmi
.word reset
.word irq

;*****************************************************************
; 6502 Zero Page Memory (256 bytes)
;*****************************************************************

.segment "ZEROPAGE"

time: .res 1
lasttime: .res 1

;*****************************************************************
; Sprite OAM Data area - copied to VRAM in NMI routine
;*****************************************************************

.segment "OAM"
oam: .res 256	; sprite OAM data

;*****************************************************************
; Include NES Function Library
;*****************************************************************

.include "neslib.s"

;*****************************************************************
; Remainder of normal RAM area
;*****************************************************************

.segment "BSS"
palette: .res 32 ; current palette buffer

;*****************************************************************
; Main application entry point for starup/reset
;*****************************************************************

.segment "CODE"
.proc reset
	sei			; mask interrupts
	lda #0
	sta PPU_CONTROL	; disable NMI
	sta PPU_MASK	; disable rendering
	sta APU_DM_CONTROL	; disable DMC IRQ
	lda #40
	sta JOYPAD2		; disable APU frame IRQ

	cld			; disable decimal mode
	ldx #$FF
	txs			; initialise stack

	; wait for first vBlank
	bit PPU_STATUS
wait_vblank:
	bit PPU_STATUS
	bpl wait_vblank

	; clear all RAM to 0
	lda #0
	ldx #0
clear_ram:
	sta $0000,x
	sta $0100,x
	sta $0200,x
	sta $0300,x
	sta $0400,x
	sta $0500,x
	sta $0600,x
	sta $0700,x
	inx
	bne clear_ram

	; place all sprites offscreen at Y=255
	lda #255
	ldx #0
clear_oam:
	sta oam,x
	inx
	inx
	inx
	inx
	bne clear_oam

; wait for second vBlank
wait_vblank2:
	bit PPU_STATUS
	bpl wait_vblank2
	
	; NES is initialized and ready to begin
	; - enable the NMI for graphical updates and jump to our main program
	lda #%10001000
	sta PPU_CONTROL
	jmp main
.endproc

;*****************************************************************
; NMI Routine - called every vBlank
;*****************************************************************

.segment "CODE"
.proc nmi
	; save registers
	pha
	txa
	pha
	tya
	pha

	; incrememt our time tick counter
	inc time

	bit PPU_STATUS
	; transfer sprite OAM data using DMA
	lda #>oam
	sta SPRITE_DMA

	; transfer current palette to PPU
	vram_set_address $3F00
	ldx #0 ; transfer the 32 bytes to VRAM
@loop:
	lda palette, x
	sta PPU_VRAM_IO
	inx
	cpx #32
	bcc @loop

	; write current scroll and control settings
	lda #0
	sta PPU_VRAM_ADDRESS1
	sta PPU_VRAM_ADDRESS1
	lda ppu_ctl0
	sta PPU_CONTROL
	lda ppu_ctl1
	sta PPU_MASK

	; flag PPU update complete
	ldx #0
	stx nmi_ready

	; restore registers and return
	pla
	tay
	pla
	tax
	pla
	rti
.endproc

;*****************************************************************
; IRQ Clock Interrupt Routine
;*****************************************************************

.segment "CODE"
irq:
	rti

;*****************************************************************
; Main application logic section includes the game loop
;*****************************************************************
 .segment "CODE"
 .proc main
 	; main application - rendering is currently off

 	; initialize palette table
 	ldx #0
paletteloop:
	lda default_palette, x
	sta palette, x
	inx
	cpx #32
	bcc paletteloop

 	; draw the title screen
	jsr display_title_screen

	; set our game settings
	lda #VBLANK_NMI|BG_0000|OBJ_1000
   	sta ppu_ctl0
   	lda #BG_ON|OBJ_ON
   	sta ppu_ctl1

	jsr ppu_update

	; wait for a gamepad button to be pressed
titleloop:
	jsr gamepad_poll
	lda gamepad
	and #PAD_A|PAD_B|PAD_START|PAD_SELECT
	beq titleloop

	; draw the game screen
	jsr display_game_screen

	; display the player's ship
	; set the Y position (byte 0) of all four parts of the player ship
	lda #196
	sta oam
	sta oam+4
	lda #204
	sta oam+8
	sta oam+12
	; set the index number (byte 1) of the sprite pattern
	ldx #0
	stx oam+1
	inx
	stx oam+5
	inx
	stx oam+9
	inx
	stx oam+13
	; set the sprite attributes (byte 2)
	lda #%00000000
	sta oam+2
	sta oam+6
	sta oam+10
	sta oam+14
	; set the X position (byte 3)  of all four parts of the player ship
	lda #120
	sta oam+3
	sta oam+11
	lda #128
	sta oam+7
	sta oam+15

	jsr ppu_update

mainloop:
	lda time
	; ensure the time has actually changed
	cmp lasttime
	beq mainloop
	; time has changed update the lasttime value
	sta lasttime

	jsr player_actions
	jsr move_player_bullet

 	jmp mainloop
.endproc

;*****************************************************************
; Check for the game controller, move the player or fire a bullet
;*****************************************************************
.segment "CODE"

.proc player_actions
	jsr gamepad_poll
	lda gamepad
	and #PAD_L
	beq not_gamepad_left
		; game pad has been pressed left
		lda oam + 3 ; get current x of ship
		cmp #0
		beq not_gamepad_left
		; subtract 1 from the ship position
		sec
		sbc #2
		; update the four sprites that make up the ship
		sta oam + 3
		sta oam + 11
		clc
		adc #8
		sta oam + 7
		sta oam + 15
		
not_gamepad_left:
	lda gamepad
	and #PAD_R
	beq not_gamepad_right
		; gamepad has been pressed right
		lda oam + 3 ; get current X of ship
		clc
		adc #12 ; allow with width of ship
		cmp #254
		beq not_gamepad_right
		lda oam + 3 ; get current X of ship
		clc
		adc #2
		; update the four sprites that make up the ship
		sta oam + 3
		sta oam + 11
		clc
		adc #8
		sta oam + 7
		sta oam + 15
		
not_gamepad_right:
	lda gamepad
	and #PAD_A
	beq not_gamepad_a
		; gamepad A button has been pressed
		lda oam + 16 ; get Y of player bullet
		cmp #$FF ; see if the sprite is not in use
		bne not_gamepad_a
			; sprite is available, place bullet
			lda #192
			sta oam + 16 ; set bullet Y
			lda #4
			sta oam + 17 ; set sprite pattern 4
			lda #0
			sta oam + 18 ; set attributes
			lda oam + 3 ; get player X position
			clc
			adc #6 ; centre bullet on player
			sta oam + 19 ; set bullet X position

not_gamepad_a:	

	rts
.endproc

;*****************************************************************
; Check for the game controller, move the player or fire a bullet
;*****************************************************************
.segment "CODE"

.proc move_player_bullet
	lda oam + 16
	cmp #$FF ; see if bullet sprite is on screen
	beq @exit
		; bullet is on screen, move it up
		sec
		sbc #4
		sta oam + 16
		bcs @exit
			; value carried so we have gone off the top of the screen
			; hide bullet
			lda #$FF
			sta oam + 16

@exit:
	rts
.endproc

;*****************************************************************
; Display Title Screen
;*****************************************************************
 .segment "CODE"

title_text:
.byte "M E G A  B L A S T",0

press_play_text:
.byte "PRESS FIRE TO BEGIN",0

title_attributes:
.byte %00000101,%00000101,%00000101,%00000101
.byte %00000101,%00000101,%00000101,%00000101

.proc display_title_screen
	jsr ppu_off ; Wait for the screen to be drawn and then turn off drawing

	jsr clear_nametable ; Clear the 1st name table

	; Write our title text
	vram_set_address (NAME_TABLE_0_ADDRESS + 4 * 32 + 6)
	assign_16i text_address, title_text
	jsr write_text

	; Write our press play text
	vram_set_address (NAME_TABLE_0_ADDRESS + 20 * 32 + 6)
	assign_16i text_address, press_play_text
	jsr write_text

	; Set the title text to use the 2nd palette entries
	vram_set_address (ATTRIBUTE_TABLE_0_ADDRESS + 8)
	assign_16i paddr, title_attributes
	ldy #0
loop:
	lda (paddr),y
	sta PPU_VRAM_IO
	iny
	cpy #8
	bne loop

	jsr ppu_update ; Wait until the screen has been drawn

	rts
.endproc

;*****************************************************************
; Display Main Game Screen
;*****************************************************************

.segment "RODATA"
; put the data in our data segment of the ROM
game_screen_mountain:
.byte 001,002,003,004,001,002,003,004,001,002,003,004,001,002,003,004
.byte 001,002,003,004,001,002,003,004,001,002,003,004,001,002,003,004
game_screen_scoreline:
.byte "SCORE 0000000"

.segment "ZEROPAGE"

paddr: .res 2 ; 16-bit address pointer

.segment "CODE"
.proc display_game_screen
	jsr ppu_off ; Wait for the screen to be drawn and then turn off drawing

	jsr clear_nametable ; Clear the 1st name table

	; output mountain line
	vram_set_address (NAME_TABLE_0_ADDRESS + 22 * 32)
	assign_16i paddr, game_screen_mountain
	ldy #0
loop:
	lda (paddr),y
	sta PPU_VRAM_IO
	iny
	cpy #32
	bne loop

	; draw a base line
	vram_set_address (NAME_TABLE_0_ADDRESS + 26 * 32)
	ldy #0
	lda #9 ; tile number to repeat
loop2:
	sta PPU_VRAM_IO
	iny
	cpy #32
	bne loop2

	; output the score section on the next line
	assign_16i paddr, game_screen_scoreline
	ldy #0
loop3:
	lda (paddr),y
	sta PPU_VRAM_IO
	iny
	cpy #13
	bne loop3

	jsr ppu_update ; Wait until the screen has been drawn
	rts
.endproc

;*****************************************************************
; Our default palette table has 16 entries for tiles and 16 entries for sprites
;*****************************************************************

.segment "RODATA"
default_palette:
.byte $0F,$15,$26,$37 ; bg0 purple/pink
.byte $0F,$19,$29,$39 ; bg1 green
.byte $0F,$11,$21,$31 ; bg2 blue
.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$28,$21,$11 ; sp0 player
.byte $0F,$14,$24,$34 ; sp1 purple
.byte $0F,$1B,$2B,$3B ; sp2 teal
.byte $0F,$12,$22,$32 ; sp3 marine
