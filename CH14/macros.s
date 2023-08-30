
;******************************************************************************
; Set the ram address pointer to the specified address
;******************************************************************************
.macro assign_16i dest, value

   lda #<value
   sta dest+0
   lda #>value
   sta dest+1

.endmacro

;******************************************************************************
; Set the vram address pointer to the address specified by the pointer
;******************************************************************************
.macro vram_set_address_i addresspointer

    lda PPU_STATUS
	lda addresspointer+1
	sta PPU_VRAM_ADDRESS2
	lda addresspointer+0
	sta PPU_VRAM_ADDRESS2

.endmacro

;******************************************************************************
; Set the vram address pointer to the specified address
;******************************************************************************
.macro vram_set_address newaddress

   lda PPU_STATUS
   lda #>newaddress
   sta PPU_VRAM_ADDRESS2
   lda #<newaddress
   sta PPU_VRAM_ADDRESS2

.endmacro

;******************************************************************************
; clear the vram address pointer
;******************************************************************************
.macro vram_clear_address

   lda #0
   sta PPU_VRAM_ADDRESS2
   sta PPU_VRAM_ADDRESS2

.endmacro

;******************************************************************************
; Adds an 8-bit value to a 16-bit value
;******************************************************************************
.macro add_16_8 dest, value

	lda value
   bmi :+
		clc
		adc dest
		sta dest
		lda dest+1
		adc #0
		sta dest+1
      jmp :++
   :

		clc
		adc dest
		sta dest
		lda dest+1
		adc #$FF
		sta dest+1
   :

.endmacro
