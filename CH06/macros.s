
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
; Set the vram address pointer to the specified address
;******************************************************************************
.macro vram_set_address newaddress

   lda PPU_STATUS
   lda #>newaddress
   sta PPU_VRAM_ADDRESS2
   lda #<newaddress
   sta PPU_VRAM_ADDRESS2

.endmacro
