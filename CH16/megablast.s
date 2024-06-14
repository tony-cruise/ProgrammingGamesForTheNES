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

time: .res 2
lasttime: .res 1
level: .res 1
animate: .res 1
enemycooldown: .res 1
temp: .res 10
score: .res 3
update: .res 1
highscore: .res 3
lives: .res 1
player_dead: .res 1
flash: .res 1
shake: .res 1
enemycount: .res 1
displaylevel: .res 1
next_extra_life: .res 1

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
; Include Sound Engine, Sound Effects and Music Data
;*****************************************************************

.segment "CODE"

; FamiStudio config.
FAMISTUDIO_CFG_EXTERNAL       = 1
FAMISTUDIO_CFG_DPCM_SUPPORT   = 1
FAMISTUDIO_CFG_SFX_SUPPORT    = 1 
FAMISTUDIO_CFG_SFX_STREAMS    = 2
FAMISTUDIO_CFG_EQUALIZER      = 1
FAMISTUDIO_USE_VOLUME_TRACK   = 1
FAMISTUDIO_USE_PITCH_TRACK    = 1
FAMISTUDIO_USE_SLIDE_NOTES    = 1
FAMISTUDIO_USE_VIBRATO        = 1
FAMISTUDIO_USE_ARPEGGIO       = 1
FAMISTUDIO_CFG_SMOOTH_VIBRATO = 1
FAMISTUDIO_USE_RELEASE_NOTES  = 1
FAMISTUDIO_DPCM_OFF           = $e000

; CA65-specifc config.
.define FAMISTUDIO_CA65_ZP_SEGMENT   ZEROPAGE
.define FAMISTUDIO_CA65_RAM_SEGMENT  BSS
.define FAMISTUDIO_CA65_CODE_SEGMENT CODE

.include "famistudio_ca65.s"

.include "megablast-sfx.s"
.include "megablast-music.s"

.segment "DPCM"
.incbin "megablast-music.dmc"

.segment "ZEROPAGE"

sfx_channel: .res 1 ; sound effects channel to use


;*****************************************************************
; Remainder of normal RAM area
;*****************************************************************

.segment "BSS"
palette: .res 32 ; current palette buffer
enemydata: .res 100 ; enemy tracking data

;*****************************************************************
; Main application entry point for starup/reset
;*****************************************************************

.segment "CODE"
.proc reset
	sei			; mask interrupts
	lda #0
	sta PPU_CONTROL	; disable NMI
	sta PPU_MASK	; disable rendering
	sta APU_CLOCK	; disbale APU sound
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

gameovertext:
.byte " G A M E  O V E R",0

leveltext:
.byte " L E V E L  ",0

.proc nmi
	; save registers
	pha
	txa
	pha
	tya
	pha

	; increment our time tick counter
	inc time
	bne :+
		inc time+1
	:

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

	lda #%00000001 ; has the score updated?
	bit update
	beq @skipscore
		jsr display_score ; display score
		lda #%11111110 ; reset score update flag
		and update
		sta update
@skipscore:
	lda #%00000010 ; has the high score updated?
	bit update
	beq @skiphighscore
		jsr display_highscore ; display high score
		lda #%11111101 ; reset high score update flag
		and update
		sta update
@skiphighscore:
	lda #%00000100 ; display the players lives
	bit update
	beq @skiplives
		jsr display_lives
		lda #%11111011
		and update
		sta update
@skiplives:
	lda #%00001000 ; does the game over message need to be displayed?
	bit update
	beq @skipgameover
		vram_set_address (NAME_TABLE_0_ADDRESS + 14 * 32 + 7)
		assign_16i text_address, gameovertext 
		jsr write_text
		lda #%11110111 ; reset game over message update flag
		and update
		sta update
@skipgameover:
	lda #%00010000 ; does the level message need to be displayed?
	bit update
	beq @skipdisplaylevel
		vram_set_address (NAME_TABLE_0_ADDRESS + 14 * 32 + 9)
		assign_16i text_address, leveltext 
		jsr write_text
		lda level ; transform each decimal digit of the level
		jsr dec99_to_bytes
		stx temp
		sta temp+1
		lda temp
		clc
		adc #48
		sta PPU_VRAM_IO 
		lda temp+1
		clc
		adc #48
		sta PPU_VRAM_IO 
		lda #%11101111 ; reset level message update flag
		and update
		sta update
@skipdisplaylevel:
	lda #%00100000 ; does the level message need to be removed?
	bit update
	beq @skipremovedisplaylevel
		vram_set_address (NAME_TABLE_0_ADDRESS + 14 * 32 + 9)
		ldx #0
		lda #0
		:
			sta PPU_VRAM_IO 
			inx
			cpx #18
			bne :-

		lda #%11011111 ; reset level message update flag
		and update
		sta update
@skipremovedisplaylevel:

	jsr animate_stars

	; write current scroll and control settings
	lda shake ; shake the screen from side to side?
	beq :+
		dec shake
		and #%1
		asl a
		asl a
	:
	sta PPU_VRAM_ADDRESS1
	;lda #0 ; no vertical scrolling
	sta PPU_VRAM_ADDRESS1
	lda ppu_ctl0 ; write the current screen settings
	sta PPU_CONTROL
	lda ppu_ctl1
	sta PPU_MASK

	; call famistudio play routine
	jsr famistudio_update

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
	lda #1 ; set initial high score to 1000
	sta highscore+1

   lda #1 ; NTSC 
   ldx #.lobyte(music_data_untitled)
   ldy #.hibyte(music_data_untitled) 
   jsr famistudio_init

   ldx #.lobyte(sounds) ; set address of sound effects
   ldy #.hibyte(sounds)
   jsr famistudio_sfx_init

 	; initialize palette table
 	ldx #0
paletteloop:
	lda default_palette, x
	sta palette, x
	inx
	cpx #32
	bcc paletteloop

resetgame:
	jsr clear_sprites

 	; draw the title screen
	jsr display_title_screen

	; set our game settings
	lda #VBLANK_NMI|BG_0000|OBJ_1000
   	sta ppu_ctl0
   	lda #BG_ON|OBJ_ON
   	sta ppu_ctl1

	jsr ppu_update

	lda #0 ; Play 1st song
	jsr play_music

	; wait for a gamepad button to be pressed
titleloop:
	jsr gamepad_poll
	lda gamepad
	and #PAD_A|PAD_B|PAD_START|PAD_SELECT
	beq titleloop

	; set our random seed based on the time counter since the splash screen was displayed
	lda time
	sta SEED0
	lda time+1
	sta SEED0+1
	jsr randomize
	sbc time+1
	sta SEED2   
	jsr randomize
	sbc time
	sta SEED2+1

	; set up ready for a new game
	lda #1
	sta level
	jsr setup_level

	lda #0 ; reset the player's score
	sta score
	sta score+1
	sta score+2

	lda #5 ; set the players starting lives
	sta lives
	lda #1
	sta next_extra_life ; set the 1st extra life to a score over 5k
	lda #0 ; reset our player_dead flag
	sta player_dead 

	; draw the game screen
	jsr display_game_screen

	; display the player's ship
	jsr display_player

	lda #64
	sta displaylevel
	lda #%00010001 ; set flag so the current score and level will be displayed
	ora update
	sta update

	jsr ppu_update

	lda #1 ; Play 2nd song
	jsr play_music

mainloop:
	lda time
	; ensure the time has actually changed
	cmp lasttime
	beq mainloop
	; time has changed update the lasttime value
	sta lasttime

	lda lives
	bne @notgameover
	lda player_dead
	cmp #1
	beq @notgameover
	cmp #240 ; we have waited long enough, jump back to the title screen 
	beq resetgame
	cmp #20
	bne @notgameoversetup
	lda #%00001000 ; signal to display Game Over message
	ora update
	sta update
@notgameoversetup:
	inc player_dead
	jmp mainloop
@notgameover:

	jsr player_actions
	jsr move_player_bullet
	jsr spawn_enemies
	jsr move_enemies

	lda displaylevel
	beq @nodisplaylevelcountdown
		dec displaylevel
		bne @nodisplaylevelcountdown
		lda #%00100000 ; signal to erase level message
		ora update
		sta update
	

@nodisplaylevelcountdown:

	lda time
	and #%111
	bne @nopalettechange
		ldx palette+1
		lda palette+2
		sta palette+1
		stx palette+2

@nopalettechange:

	lda flash
	beq @noflash
		dec flash
		lda palette
		cmp #$0f
		bne @noflash
			lda #$30
			sta palette
			sta palette+16
			jmp mainloop
@noflash:
	lda #$f
	sta palette
	sta palette+16

 	jmp mainloop
.endproc

;*********************************************************
; Play a sound effect
; a = sound effect to play
; sfx_channel = sound effects channel to use
;*********************************************************/
.segment "CODE"

.proc play_sfx
   sta temp+9 ; save sound effect number
   tya ; save current register values
   pha
   txa
   pha

   lda temp+9 ; get the sound effect number
   ldx sfx_channel ; choose the channel to play the sound effect on
   jsr famistudio_sfx_play

   pla ; restore register values
   tax
   pla
   tay
   rts
.endproc

;*********************************************************
; Play a music track
; a = number of the music track to play
;*********************************************************/
.segment "CODE"

.proc play_music
	sta temp+9 ; save music track number
	tya ; save current register values
	pha
	txa
	pha

	lda temp+9 ; get music track to play
	jsr famistudio_music_play

	pla ; restore register values
	tax
	pla
	tay
	rts
.endproc

.segment "CODE"
.proc display_lives
	vram_set_address (NAME_TABLE_0_ADDRESS + 27 * 32 + 14)
	ldx lives
	beq @skip ; no lives to display
	and #%00000111 ; limit to a max of 8
@loop:
	lda #5
	sta PPU_VRAM_IO
	lda #6
	sta PPU_VRAM_IO
	dex
	bne @loop

@skip:
	lda #8 ; blank out the remainder of the row
	sec
	sbc lives
	bcc @skip2
	tax
	lda #0
@loop2:
	sta PPU_VRAM_IO
	sta PPU_VRAM_IO
	dex
	bne @loop2
@skip2:

	vram_set_address (NAME_TABLE_0_ADDRESS + 28 * 32 + 14)
	ldx lives
	beq @skip3 ; no lives to display
	and #%00000111 ; limit to a max of 8
@loop3:
	lda #7
	sta PPU_VRAM_IO
	lda #8
	sta PPU_VRAM_IO
	dex
	bne @loop3	

@skip3:
	lda #8 ; blank out the remainder of the row
	sec
	sbc lives
	bcc @skip4
	tax
	lda #0
@loop4:
	sta PPU_VRAM_IO
	sta PPU_VRAM_IO
	dex
	bne @loop4
@skip4:

	rts
.endproc

.segment "CODE"

.proc spawn_enemies
	ldx enemycooldown ; decrement enemy cool down
	dex
	stx enemycooldown
	cpx #0
	beq :+
		rts
	:
	ldx #1 ; set short cool down
	stx enemycooldown
	lda level ; get the current level
	clc
	adc #1 ; increment by 1
	asl
	asl ; multiply by 4 by shifting left twice
	sta temp ; save our value
	jsr rand ; get next random value
	tay ; transfer the value into the y register
	;lda temp ; get back our calculated value
	cpy temp
	bcc :+ ; continue if random value less than our calculated value
	rts
:
	ldx #20 ; set new cool down period
	stx enemycooldown
	; now see if the is an enemy object available
	ldy #0 ; counter
	sty temp
@loop:
	lda enemydata,y
	beq :+
	tya ; add ten to Y
	clc
	adc #10
	tay
	inc temp
	lda temp
	cmp #10
	bne @loop
	; did not find an enemy to use
	rts
:
	sty temp+1 ; save y

	inc enemycount
	lda #40
	cmp enemycount
	bne @notendoflevel
		inc level
		lda #64
		sta displaylevel
		lda #0
		sta enemycount
		lda #%00010000 ; set flag so the current level will be displayed
		ora update
		sta update

@notendoflevel:

	; determine the type of enemy to select
	jsr rand
	ldy temp+1
	and #%1111
	cmp #$0f
	bne @notSmartBomb
	lda #3 ; set the enemy type as Smartbomb
	jmp @setEnemyType
@notSmartBomb:
	and #%1 ; A will be zero or one
	clc
	adc #1 ; A will be 1 (larger meteor) or 2 (small meteor)
	
@setEnemyType:
	sta enemydata,y ; mark the enemy as in use and set it's type

	; get the enemy data
	sec
	sbc #1
	sta temp+1 ; save as our loop counter
	beq @skipMultiply ; skip if zero
	lda #0
	clc ; multiply by 9
@loop5:
	adc #9
	dec temp+1
	bne @loop5
@skipMultiply:
	tax

	; save Y
	tya
	pha

	iny
	; now copy 9 bytes
	lda #9
	sta temp+1
@loop4:
	lda enemy_source_data,x
	sta enemydata,y
	inx
	iny
	dec temp+1
	bne @loop4

	; restore Y
	pla
	tay
	lda enemydata,y

	; calculate first sprite oam position
	lda temp
	asl ; multiply by 16
	asl
	asl
	asl
	clc
	adc #20 ; skip first five sprites
	tax

	lda enemydata+4,y
	beq @noAdjustX
	sty temp+1
	jsr rand
	ldy temp+1
	and #%1
	beq @noAdjustX
	lda enemydata+4,y
	eor #$ff ; make negative
	clc
	adc #$01
	sta enemydata+4,y

@noAdjustX:

	; now setup the enemy sprite
	lda enemydata+3,y ; get the number of sprites used
	cmp #1
	bne @fourSprites

	; only one sprite used
	; set the Y position (byte 0) 
	lda #0
	sta oam,x
	lda #$ff ; ensure none of the other sprites in the group are visible
	sta oam+4,x
	sta oam+8,x
	sta oam+12,x
	; set the index number (byte 1) of the sprite pattern
	lda enemydata+1,y ; get starting pattern
	sta oam+1,x
	; set the sprite attributes (byte 2)
	lda enemydata+9,y
	sta oam+2,x
	; set the X position (byte 3)
	jsr rand
	and #%01110000
	clc
	adc #48
	sta oam+3,x

	rts

@fourSprites:
	; set the Y position (byte 0)
	lda #0
	sta oam,x
	sta oam+4,x
	lda #8
	sta oam+8,x
	sta oam+12,x
	; set the index number (byte 1) of the sprite pattern
	lda enemydata+1,y ; get starting pattern
	sta oam+1,x
	clc
	adc #1
	sta oam+5,x
	adc #1
	sta oam+9,x
	adc #1
	sta oam+13,x
	; set the sprite attributes (byte 2)
	lda enemydata+9,y
	sta oam+2,x
	sta oam+6,x
	sta oam+10,x
	sta oam+14,x
	; set the X position (byte 3)
	jsr rand
	and #%11110000
	clc
	adc #48
	sta oam+3,x
	sta oam+11,x
	clc
	adc #8
	sta oam+7,x
	sta oam+15,x

	rts
.endproc

.segment "CODE"

.proc move_enemies

	; setup for collision detection of bullet with enemies
	lda oam+16 ; get bullet y
	sta cy1
	lda oam+19 ; get bullet x
	sta cx1
	lda #4 ; bullet is four pixels high
	sta ch1
	lda #1 ; bullet is one pixel wide
	sta cw1

	ldy #0
	lda #0
	sta temp+2 ; initialise loop counter
@loop:
	lda enemydata,y
	bne :+
		jmp @skip
	:

	; enemy is on screen
	; calculate first sprite oam position
	lda temp+2
	asl ; multiply by 16
	asl
	asl
	asl
	clc
	adc #20 ; skip first five sprites
	tax

	lda enemydata,y
	cmp #3 ; is it a smart bomb
	bne @notSmartBomb
	lda enemydata+4,y ; get DX
	and #%10000000
	beq @movingRight
	; smart bomb moving left
	lda oam+3 ; get player X
	cmp oam+3,x
	bcc @notSmartBomb ; is smart bomb to the left of the player?
	sec
	sbc oam+3,x
	cmp #32
	bcc @notSmartBomb
	lda enemydata+4,y
	eor #$ff ; make negative
	clc
	adc #$01
	sta enemydata+4,y
	jmp @notSmartBomb
@movingRight:
	; smart bomb moving right
	lda oam+3 ; get player X
	clc
	adc #12 ; adjust for width of player
	cmp oam+3,x 
	bcs @notSmartBomb ; is smart bomb to the right of the player?
	lda oam+3,x
	sec
	sbc oam+3 ; get the difference between the two
	cmp #44 ; is it within 32 pixels
	bcc @notSmartBomb
	lda enemydata+4,y
	eor #$ff ; make negative
	clc
	adc #$01
	sta enemydata+4,y

@notSmartBomb:

	; adjust the enemy X
	lda enemydata+4,y
	beq @noMoveX
		clc
		adc oam+3,x
		sta oam+3,x
		sta oam+11,x
		clc
		adc #8
		sta oam+7,x
		sta oam+15,x

@noMoveX:

	lda oam,x ; get enemy Y
	clc
	adc enemydata+5,y ; add change in Y from table
	sta oam,x ; save the new Y position
	clc
	adc enemydata+8,y ; add on the enemy's height
	cmp #204
	bcc @nohitbottom
	; has reached the ground
	lda #255
	sta oam,x ; hide all sprites
	sta oam+4,x
	sta oam+8,x
	sta oam+12,x
	
	lda enemydata,y
	cmp #3
	bne @notSmartBomb2
		lda #32
		sta flash
		sta shake

		lda #FAMISTUDIO_SFX_CH1
		sta sfx_channel
		lda #2 ; play big boom sound effect
		jsr play_sfx

@notSmartBomb2:

	lda #0 ; clear the enemies in use flag
	sta enemydata,y

	; check score is not already zero
	clc
	lda score
	adc score+1
	adc score+2
	bne :+
		jmp @skip
	:
	lda #1 ; subtract 10 from the score
	jsr subtract_score
	jmp @skip

@nohitbottom:

	lda enemydata+3,y
	cmp #1 ; does the enemy only have one pattern
	beq :+
	lda oam,x ; update the other sprite Y positions
	sta oam+4,x
	clc
	adc #8
	sta oam+8,x
	sta oam+12,x
:


	lda time
	and #%11 ; only animate every four frames
	bne @noanimate
		; does the enemy have more than one pattern?
		lda enemydata+1,y ; get starting pattern
		cmp enemydata+2,y
		beq @noanimate
			lda enemydata+3,y
			cmp #1 ; does the enemy only have one sprite
			beq @singleSprite
				lda oam+1,x ; get the 1st sprite pattern number
				clc
				adc #4 ; go to the next pattern
				cmp enemydata+2,y
				beq :+
				bcc :+
					lda enemydata+1,y ; past the end pattern, get the starting pattern
				:
				sta oam+1,x
				clc
				adc #1
				sta oam+5,x ; update the other patterns
				clc
				adc #1
				sta oam+9,x
				clc
				adc #1
				sta oam+13,x
				jmp @noanimate
			@singleSprite:
				lda oam+1,x ; get the 1st sprite pattern number
				clc
				adc #1 ; go to the next pattern
				cmp enemydata+2,y
				beq :+
				bcc :+
					lda enemydata+1,y ; past the end pattern, get the starting pattern
				:
				sta oam+1,x

@noanimate:

	lda player_dead
	cmp #0 ; check the player is not currently dead
	bne @notlevelwithplayer
	lda oam,x ; get enemy Y
	clc
	adc enemydata+8,y ; add on the enemies height
	cmp #$c4 ; is the enemy level with the player
	bcc @notlevelwithplayer

	lda oam+3 ; get the players X position	
	clc
	adc #12 ; add on the width of the player
	cmp oam+3,x ; is the enemies X larger than the player plus it's width?
	bcc @notlevelwithplayer

	lda oam+3,x ; get the enemy X position
	clc
	adc enemydata+7,y ; add on it's width
	cmp oam+3 ; is the enemies X plus it's width smaller than the player's X position
	bcc @notlevelwithplayer

	dec lives ; decrease our lives counter
	lda #%00000100 ; set flag so the current lives will be displayed
	ora update
	sta update

	lda #FAMISTUDIO_SFX_CH1
	sta sfx_channel
	lda #2 ; play big boom sound effect
	jsr play_sfx

	lda #1 ; mark the player as currently dead
	sta player_dead

	lda #$ff
	sta oam,x ; erase enemy
	sta oam+4,x
	sta oam+8,x
	sta oam+12,x
	lda #0 ; clear enemy's data flag
	sta enemydata,y
	jmp @skip

@notlevelwithplayer:

	lda oam+16
	cmp #$FF ; is player bullet on screen
	beq @skip

	lda oam,x ; get enemy y position
	sta cy2
	lda oam+3,x ; get enemy x position
	sta cx2
	lda enemydata+7,y ; set enemy width
	sta cw2
	lda enemydata+8,y ; set enemy height
	sta ch2
	jsr collision_test
	bcc @skip

	; bullet has hit meteor
	lda #$ff
	sta oam+16 ; erase player bullet
	sta oam,x ; erase enemy
	sta oam+4,x
	sta oam+8,x
	sta oam+12,x
	lda #0 ; clear enemy's data flag
	sta enemydata,y

	lda enemydata+6,y ; add enemy points to the score
	jsr add_score

	lda #FAMISTUDIO_SFX_CH1
	sta sfx_channel
	lda #1 ; play boom sound effect
	jsr play_sfx


@skip:
	tya ; goto next enemy
	clc
	adc #10
	tay
	inc temp+2
	lda temp+2
	cmp #10
	beq :+
		jmp @loop
	:

	rts
.endproc

;*****************************************************************
; Randomly display star characters
;*****************************************************************

.segment "ZEROPAGE"

starlocations:	.res 10*2 ; two bytes per star

.segment "CODE"

.proc place_stars
	lda #0 ; clear star locations
	ldx #0
@loop:
	sta starlocations,x
	inx
	cpx #20
	bne @loop

	ldx #0
@loop2:
	jsr rand
	pha ; save A
	and #%1111 ; get lower nibble
	clc
	adc #1 ; skip zero
	sta temp ; this is the row of our star
	pla ; get A back
	lsr ; move upper nibble into the lower nibble + multiple by 2
	lsr
	lsr
	sta temp+1 ; this is the column of our star

	assign_16i paddr, NAME_TABLE_0_ADDRESS+64 ; start from the 2nd row
@loop3:
	add_16_8 paddr, #32 ; add 32 for each row
	dec temp
	bne @loop3

	add_16_8 paddr, temp+1 ; add the column

	vram_set_address_i paddr
	lda #12 ; output star pattern
	sta PPU_VRAM_IO

	lda paddr ; save our address for later
	sta starlocations,x
	lda paddr+1
	sta starlocations+1,x

	inx
	inx
	cpx #20
	beq :+
		jmp @loop2
	:

	rts
.endproc

;*****************************************************************
; Randomly animate stars by changing their pattern
;*****************************************************************

.segment "CODE"

.proc animate_stars
	lda time
	and #%11
	beq :+
		rts ; any change every four ticks
	:
	
	ldx #0
	lda starlocations,x
	bne @loop
		rts ; skip processing if there are no stars
@loop:
    lda PPU_STATUS
	lda starlocations+1,x
   	sta PPU_VRAM_ADDRESS2
	lda starlocations,x
   	sta PPU_VRAM_ADDRESS2

	lda time
	lsr
	lsr
	and #%1 ; use the 3rd bit of time to control our pattern
	clc
	adc #12
	sta PPU_VRAM_IO

	inx
	inx
	cpx #20
	bne @loop
	rts
.endproc

;*****************************************************************
; Get setup for a new level
;*****************************************************************
.segment "CODE"

.proc setup_level 	
	lda #0 ; clear enemy data
	ldx #0
@loop:
	sta enemydata,x
	inx
	cpx #100
	bne @loop
	lda #20 ; set initial enemy cool down
	sta enemycooldown
	
	lda #$ff ; hide all enemy sprites
	ldx #0
@loop2:
	sta oam+20,x
	inx
	cpx #160
	bne @loop2

	lda #0 ; reset our enemy count
	sta enemycount
	lda #64
	sta displaylevel
	lda #%00010000 ; set flag so the current level will be displayed
	ora update
	sta update

	rts
.endproc

;*****************************************************************
; add_score: Add to the players score
; Parameters:
; a = value to add to the score
;*****************************************************************
.segment "CODE"
.proc add_score
	clc
	adc score ; add the value in a to the 1st byte of the score
	sta score
	cmp #100
	bcc @skip

	sec ; 1st byte has exceeded 99, handle overflow
	sbc #100
	sta score
	inc score+1
	lda score+1
	cmp #100
	bcc @skip

	sec ; 2nd byte has exceeded 99, handle overflow
	sbc #100
	sta score+1
	inc score+2
	lda score+2
	cmp #100
	bcc @skip
	sec ; if 3rd byte has exceeded 99, adjust and discard overflow
	sbc #100
	sta score+2
	
@skip:
	lda #%000000001 ; set flag to write score to the screen
	ora update
	sta update

	lda highscore+2
	cmp score+2
	bcc @highscore
	bne @nothighscore

	lda highscore+1
	cmp score+1
	bcc @highscore
	bne @nothighscore

	lda highscore
	cmp score
	bcs @nothighscore

@highscore:
	lda score
	sta highscore
	lda score+1
	sta highscore+1
	lda score+2
	sta highscore+2

	lda #%00000010 ; set flag to write high score to the screen
	ora update
	sta update

@nothighscore:

	lda next_extra_life
	cmp score+1
	bcc @extralife
	bne @notextralife
@extralife:
	clc
	lda next_extra_life
	adc #5 ; next life another 5K
	sta next_extra_life

	inc lives ; increment lives counter

	lda #%00000100 ; set flag to update lives counter on screen
	ora update
	sta update

	; it would also be nice to have a sound effect for an extra life
	; played at this stage.

@notextralife:
	rts
.endproc

;*****************************************************************
; subtract_score: Subtract from the players score
; Parameters:
; a = value to subtract from the score
;*****************************************************************
.segment "CODE"
.proc subtract_score
	sta temp ; save our a value
	sec
	lda score
	sbc temp ; subtract the value in a from the 1st byte of the score
	sta score
	bcs @skip

	clc
	adc #100 ; current value in a is negative, add to 100 to ensure we are 99 or less
	sta score
	dec score+1 ; decrement our 2nd score byte
	bcs @skip

	clc ; add 100 to ensure byte 2 is 99 or less
	lda score+1
	adc #100
	sta score+1
	dec score+2 ; decrement our 3rd score byte
	bcs @skip

	lda #0 ; ensure the score can't be less than zero
	sta score+2
	sta score+1
	sta score

@skip:
	lda #%00000001 ; set flag to write score to the screen
	ora update
	sta update
	rts
.endproc

;*****************************************************************
; display_score: Write the score to the screen
;*****************************************************************
.segment "CODE"

.proc display_score
	vram_set_address (NAME_TABLE_0_ADDRESS + 27 * 32 + 6)

	lda score+2 ; transform each decimal digit of the score
	jsr dec99_to_bytes
	stx temp
	sta temp+1

	lda score+1
	jsr dec99_to_bytes
	stx temp+2
	sta temp+3

	lda score
	jsr dec99_to_bytes
	stx temp+4
	sta temp+5

	ldx #0 ; write the six characters to the screen
@loop:
	lda temp,x
	clc
	adc #48
	sta PPU_VRAM_IO
	inx
	cpx #6
	bne @loop
	lda #48 ; write trailing zero
	sta PPU_VRAM_IO

	vram_clear_address
	rts
.endproc

;*****************************************************************
; display_highscore: Write the high score to the screen
;*****************************************************************
.segment "CODE"

.proc display_highscore
	vram_set_address (NAME_TABLE_0_ADDRESS + 1 * 32 + 13)

	lda highscore+2 ; transform each decimal digit of the high score
	jsr dec99_to_bytes
	stx temp
	sta temp+1

	lda highscore+1
	jsr dec99_to_bytes
	stx temp+2
	sta temp+3

	lda highscore
	jsr dec99_to_bytes
	stx temp+4
	sta temp+5

	ldx #0 ; write the six characters to the screen
@loop:
	lda temp,x
	clc
	adc #48
	sta PPU_VRAM_IO
	inx
	cpx #6
	bne @loop
	lda #48 ; write trailing zero
	sta PPU_VRAM_IO

	vram_clear_address
	rts
.endproc

;*****************************************************************
; Check for the game controller, move the player or fire a bullet
;*****************************************************************
.segment "CODE"

.proc set_player_shape
	stx oam+1
	inx
	stx oam+5
	inx
	stx oam+9
	inx
	stx oam+13
	rts
.endproc

.proc display_player
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
	rts
.endproc

.proc player_actions
	lda player_dead
	beq @continue
	cmp #1 ; player flagged as dead, set initial shape
	bne @notstep1
	ldx #20 ; set 1st explosion pattern
	jsr set_player_shape
	lda #$00000001 ; select 2nd palette
	sta oam+2
	sta oam+6
	sta oam+10
	sta oam+14
	jmp @nextstep

@notstep1:
	cmp #5 ; ready to change to next explosion shape
	bne @notstep2
	ldx #24 ; set 2nd explosion pattern
	jsr set_player_shape
	jmp @nextstep

@notstep2:
	cmp #10 ; ready to change to next explosion shape
	bne @notstep3
	ldx #28 ; set 3rd explosion pattern
	jsr set_player_shape
	jmp @nextstep

@notstep3:
	cmp #15 ; ready to change to next explosion shape
	bne @notstep4
	ldx #32 ; set 3rd explosion pattern
	jsr set_player_shape
	jmp @nextstep

@notstep4:
	cmp #20 ; explosion finished, reset player
	bne @nextstep
	lda lives
	cmp #0 ; check for game over
	bne @notgameover
	rts ; game over exit
@notgameover:
	jsr setup_level ; reset all enemies objects
	jsr display_player ; display the player at the starting position
	lda #0 ; clear the player dead flag
	sta player_dead
	rts
@nextstep:
	inc player_dead
	rts
@continue:
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

			lda #FAMISTUDIO_SFX_CH0
			sta sfx_channel
			lda #0 ; play zap sound effect
			jsr play_sfx
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

	lda #0 ; clear star locations
	ldx #0
@loop:
	sta starlocations,x
	inx
	cpx #20
	bne @loop

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

	jsr display_lives

	jsr place_stars

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
.byte $0F,$26,$28,$17 ; sp1 explosion
.byte $0F,$38,$28,$18 ; sp2 browns
.byte $0F,$12,$22,$32 ; sp3 marine

;*****************************************************************
; Define our four different enemy types
;*****************************************************************

.segment "RODATA"
enemy_source_data:
.byte 008,012,004,000,002,002,012,012,002 ; large meteor
.byte 036,037,001,001,003,003,008,007,002 ; small meteor
.byte 016,019,001,002,003,006,008,008,003 ; smart bomb
.byte 040,044,004,000,000,000,000,000,001 ; enemy explosion
