;-----------------------------------------------------------------------.
;                                                             Lines.asm |
;-----------------------------------------------------------------------'
; Contains Logic and variables for detecting when the user has completed
; lines, the frame step loginc to clear the lines, etc.

*=$7100

; The DETECT_COMPLETED_LINES routine stores the indexes of completed lines
; in this vector. This vector should not be touched outside of this file.
COMPLETED_LINES_VECTOR_LENGTH !byte $00
COMPLETED_LINES_VECTOR        !byte $00, $00, $00, $00

TEMP_LINE !byte $00

;-----------------------------------------------------------------------.
;                                                 Detect Complete lines |
;------------------------------------------------------------------------
; Detects any lines that are completed in the field.                    |
; A - returns the number of completed lines                             |
;-----------------------------------------------------------------------'
!zn DETECT_COMPLETED_LINES
DETECT_COMPLETED_LINES

; Reset the vector length
  lda #0
  sta COMPLETED_LINES_VECTOR_LENGTH

; Get a pointer to the field data
  lda #<FIELD_DATA
  sta POINTER1_LO
  lda #>FIELD_DATA
  sta POINTER1_HI

; Set up loop counters
  ldy #0       ; loop 20 rows
  ldx #0       ; loop 10 columns
  
; The inner loop reads at POINTER1 which is the offset of the current row
; in the field data, offset by Y which is the column
- lda (POINTER1),Y
  beq +             ; If we find a byte that is 0, meaning there is no piece part
                    ; there, we don't need to read anymore bytes in this row.
  
; Increament y, compare to 10, the number of columns
  iny
  cpy #$0A
  bne -
  
; If we make it here without the above branch firing for the entire line, then
; This line hase a piece part in every byte of the line and therefore needs to be
; added to the COMPLETED_LINES_VECTOR at the current position of COMPLETED_LINES_VECTOR_LENGTH.

; The x register actually contains the current line, we are going to need to store that
; away because we will need the x register for the vector offset
  
  stx TEMPX                             ; Store the x register
  txa                                   ; Move the x register to A
  ldx COMPLETED_LINES_VECTOR_LENGTH     ; Move the current vector length to X
  sta COMPLETED_LINES_VECTOR,x        ; Move the current row number to vector
  inc COMPLETED_LINES_VECTOR_LENGTH     ; Increment the vector length
  ldx TEMPX                             ; Restore the x register
  
  
; Increment the row
+ inx
  cpx #$14
  beq .exit     ; Exit if row = 20
  clc
  lda POINTER1_LO
  adc #$0A
  sta POINTER1_LO
  ldy #$00
  jmp -
  
  
.exit
  lda COMPLETED_LINES_VECTOR_LENGTH
  rts
  
;-----------------------------------------------------------------------.
;                                          Runs the clearing lines step |
;------------------------------------------------------------------------
; Runs a frame step when CURRENT_STATE = LINE_LINE_ANMATION             |
; A - A is the return value; 1 if the animation is not complete, 0 if   |
;     the animation is complete.                                        |
;-----------------------------------------------------------------------'
!zn RUN_CLEARING_LINES_STEP
RUN_CLEARING_LINES_STEP

  jsr REMOVE_LINES
  lda #$00
  
  rts
  
  
;-----------------------------------------------------------------------.
;                                               Remove lines from field |
;------------------------------------------------------------------------
; Removes lines in the COMPLETED_LINES_VECTOR from the FIELD_DATA       |
;-----------------------------------------------------------------------'
!zn REMOVE_LINES
REMOVE_LINES

  ldy #$00

- ldx COMPLETED_LINES_VECTOR_LENGTH
  beq +
  
  lda COMPLETED_LINES_VECTOR,y
  sty TEMP_LINE
  pha
  jsr REMOVE_LINE
  pla
  pha
  jsr REMOVE_LINE_FROM_COLOR_MEMORY
  pla
  jsr REMOVE_LINE_FROM_COLOR_FIELD
  dec COMPLETED_LINES_VECTOR_LENGTH
  ldy TEMP_LINE
  iny
  jmp -
  
+ rts
;-----------------------------------------------------------------------.
;                                                Remove line from field |
;------------------------------------------------------------------------
; Removes a line in the FIELD_DATA                                      |
; A - index of the line to remove                                       |
;-----------------------------------------------------------------------'
!zn REMOVE_LINE
REMOVE_LINE

; Multiply the index by 10 to get the offset of the line in FIELD_DATA space
  sta FAC1
  lda #$0A
  sta FAC2
  jsr MUL8
  
; The maximum value of A should be 20, and 20*10 is only 200 so we only
; need the lo byte from the result of the multiply, which is in x.

; The top line will alway be replaced with all 0's, if the requested line
; was at index 0, jump to the code for replacing the top line.
  txa         ; Move the lo byte of the result to A
  cmp #$00
  beq .REPLACE_TOP_LINE

; In order to remove a line from the FIELD DATA we need to copy the previous
; line to the line to remove. Then move to the previous line and copy the line
; before the previous line to the previous line, etc etc until we get to the
; top line, which will be filled with zeros.

; We will copy the line going from right to left. Since the result of the
; above multiply will give us the first byte in the row, we need to add 9
; to the offset to get to the right side.
  clc
  adc #$09
  
; Move the counter to the x register
  tax
  
; From here we will just copy each byte from the byte above it until we have
; copied the left byte in row 1 (row 0 is the top row). The left byte in row
; 1 is byte number 10.

- lda FIELD_DATA-10,X
  sta FIELD_DATA,X
  
  cpx #$0a
  beq .REPLACE_TOP_LINE
  dex
  jmp -
  
  
.REPLACE_TOP_LINE
  ldx #$09
  lda #$00
- sta FIELD_DATA,x
  dex
  cpx #$FF
  bne -
  
  rts
  
;-----------------------------------------------------------------------.
;                                         Remove line from color memory |
;------------------------------------------------------------------------
; Removes a line of color memory from the field                         |
; A - index of the line to remove                                       |
;-----------------------------------------------------------------------'
!zn REMOVE_LINE_FROM_COLOR_MEMORY
REMOVE_LINE_FROM_COLOR_MEMORY

; The top line will alway be replaced with all 0's, if the requested line
; was at index 0, jump to the code for replacing the top line.
  cmp #$00
  beq .REPLACE_TOP_LINE

; The field is offset from the screen in the Y direction by 3 columns, so
; we need to add 3 to the index as a first step in calculating the screen
; space offset.
  clc
  adc #$03

; Multiply the index by 40 to get the offset of the line in screen Y space
  sta FAC1
  lda #$28
  sta FAC2
  jsr MUL8
  
; Store the result of the multiplication in POINTER1
  stx POINTER1_LO
  sta POINTER1_HI
  
; Add 13 to this offset since the field is offset by this amount in SCREEN
; space.
  clc
  lda POINTER1_LO
  adc #$0D
  sta POINTER1_LO
  bcc +
  inc POINTER1_HI
+

; Move the pointer to screen space by adding COLOR_MEMORY
  clc
  lda POINTER1_LO
  adc #<COLOR_MEMORY
  sta POINTER1_LO
  lda POINTER1_HI
  adc #>COLOR_MEMORY
  sta POINTER1_HI

; Now to get a pointer to the byte directly above the previous byte.
  sec
  lda POINTER1_LO
  sbc #$28
  sta POINTER2_LO
  lda POINTER1_HI
  sbc #$00
  sta POINTER2_HI
  
; In order to remove a line from the FIELD DATA we need to copy the previous
; line to the line to remove. Then move to the previous line and copy the line
; before the previous line to the previous line, etc etc until we get to the
; top line, which will be filled with zeros.

; We will copy the line going from right to left. Since the result of the
; above math will give us the first byte in the row, we need to add 9
; to the offset to get to the right side.
  
; We will use inderect addressing, and Y will start at 9, moving to 9
  ldy #$09
  
; From here we will just copy each byte from the byte above it until we have
; copied the left byte in row 1 (row 0 is the top row). The left byte in row
; 1 is byte number 10.

- lda (POINTER2),y
  sta (POINTER1),y
  
  dey
  cpy #$ff
  bne -
  
  sec
  lda POINTER2_LO
  sta POINTER1_LO
  sbc #$28
  sta POINTER2_LO
  
  lda POINTER2_HI
  sta POINTER1_HI
  sbc #00
  sta POINTER2_HI
  
; We stop when we reach row one, which will be the x = 13, y = 3 in screen space
; address = $D800 + $03 * $28 + $0D = $D885
  
  lda POINTER1_LO
  cmp #$85
  bne +
  lda POINTER1_HI
  cmp #$D8
  bne +
  
; If we made it here POINTER1 = $D885, -> copy the last line
  jmp .REPLACE_TOP_LINE
  
+ ldy #$09
  jmp -
  
  
.REPLACE_TOP_LINE
  ldy #09
  lda #$0B
- sta (POINTER1),Y
  dey
  cpy #$FF
  bne -
  
  rts
  
;-----------------------------------------------------------------------.
;                                  Remove line from field color storage |
;------------------------------------------------------------------------
; Removes a line in the FIELD_DATA                                      |
; A - index of the line to remove                                       |
;-----------------------------------------------------------------------'
!zn REMOVE_LINE_FROM_COLOR_FIELD
REMOVE_LINE_FROM_COLOR_FIELD

; Multiply the index by 10 to get the offset of the line in FIELD_DATA space
  sta FAC1
  lda #$0A
  sta FAC2
  jsr MUL8
  
; The maximum value of A should be 20, and 20*10 is only 200 so we only
; need the lo byte from the result of the multiply, which is in x.

; The top line will alway be replaced with all 0's, if the requested line
; was at index 0, jump to the code for replacing the top line.
  txa         ; Move the lo byte of the result to A
  cmp #$00
  beq .REPLACE_TOP_LINE

; In order to remove a line from the FIELD DATA we need to copy the previous
; line to the line to remove. Then move to the previous line and copy the line
; before the previous line to the previous line, etc etc until we get to the
; top line, which will be filled with zeros.

; We will copy the line going from right to left. Since the result of the
; above multiply will give us the first byte in the row, we need to add 9
; to the offset to get to the right side.
  clc
  adc #$09
  
; Move the counter to the x register
  tax
  
; From here we will just copy each byte from the byte above it until we have
; copied the left byte in row 1 (row 0 is the top row). The left byte in row
; 1 is byte number 10.

- lda FIELD_COLOR_DATA-10,X
  sta FIELD_COLOR_DATA,X
  
  cpx #$0a
  beq .REPLACE_TOP_LINE
  dex
  jmp -
  
  
.REPLACE_TOP_LINE
  ldx #$09
  lda #$0B
- sta FIELD_COLOR_DATA,x
  dex
  cpx #$FF
  bne -
  
  rts