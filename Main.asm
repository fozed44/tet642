
!SOURCE "common.asm"

;-----------------------------------------------------------------------
;                   C-64 - T-E-T-R-I-S - C-L-O-N-E
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;                                                  BASIC AUTOSTART STUB
;-----------------------------------------------------------------------




* = $0801

        !BYTE 12,8,0,0,158
        !BYTE 48+4
        !BYTE 48+0
        !BYTE 48+9
        !BYTE 48+6



FIELD_START_X = 13
FIELD_START_Y = 03
FIELD_END_X   = 19
FIELD_END_Y   = 19

; The offset of the field from the start of screen buffer memory.
; Calclulated as (FIELD_START_Y) * 40 + FIELD_START_X
FIELD_OFFSET  = $85



PIECE_SQUARE    = $00
PIECE_L         = $0A
PIECE_L_REVERSE = $14
PIECE_T         = $1E

CURRT_PIECE_PTR    = $20
CURRT_PIECE_PTR_LO = $20
CURRT_PIECE_PTR_HI = $21

PREV_PIECE_PTR     = $22
PREV_PIECE_PTR_LO  = $22
PREV_PIECE_PTR_HI  = $23

; Width of the field in bytes
FIELD_DATA_WIDTH = $0A

; A generic piece pointer that always points to piece data, but can be
; set to any piece, depending on the operation.
GEN_PIECE_PTR    = $26;
GEN_PIECE_PTR_LO = $26;
GEN_PIECE_PTR_HI = $27;

; points to character data storage (4bytes)
; Always points to BUFFA_CHARS or BUFFB_CHARS
CHAR_DATA_PTR  = $30

; Always points to BUFFA_PIECE_LOCATION or BUFFB_PIECE_LOCATION
BUFFER_POS_PTR = $32

; The number of elements in one piece's data
PIECE_DATA_WIDTH = 8
; The number of different pieces
NUMBER_OF_PIECES = 7
; PIECE_DATA_WIDTH * NUMBER_OF_PIECES
PIECE_DATA_BLOCK_WIDTH = PIECE_DATA_WIDTH * NUMBER_OF_PIECES

;-----------------------------------------------------------------------
;                                                       Timer Resets
;-----------------------------------------------------------------------

; Timer resets - These are the values that various count down timers are
; reset to after they reach 0.

; The raster timer counts the number of raster syncs per frame. The higher
; this number, the slower the game will run.
RASTER_TIMER_RESET = $07

; When FIRE_LOCKOUT_TIMER is !0 FIRE_ACTIVE can never be set. 
; FIRE_LOCKOUT_TIMER_RESET determines how many raster resets after the
; button is pressed before it can be active again
FIRE_LOCKOUT_TIMER_RESET = $0A

; The PIECE_DROP_TIMER is used to count the raster timer resets between
; forced drops of the piece. As the level goes up the timer is reset to
; a lower and lower time. The reset values for each level are stored in
; PIECE_DROP_TIMER_RESET_VECTOR
PIECE_DROP_TIMER_RESET_VECTOR_LENGTH = 10

;-----------------------------------------------------------------------
;                                                                  MAIN
;-----------------------------------------------------------------------

*= $1000
!ZN INIT
INIT

        JSR INIT_RND_GEN

        JSR BASIC_OFF
        JSR INITIALIZE_SCREEN_MEMORY
        ;JSR SET_EXTENDED_COLOR_MODE
        JSR COPY_CUSTOM_CHARS
        JSR COPY_CUSTOM_SCREEN
        
        LDA #$0b
        JSR SET_FIELD_COLOR_DATA

        LDA #<(PIECE_DATA+PIECE_DATA_WIDTH*6)
        STA CURRT_PIECE_PTR_LO
        LDA #>(PIECE_DATA+PIECE_DATA_WIDTH*6)
        STA CURRT_PIECE_PTR_HI

        LDA #<PIECE_DATA
        STA PREV_PIECE_PTR_LO
        LDA #>PIECE_DATA
        STA PREV_PIECE_PTR_HI

        LDA $40
        JSR SET_FIELD_DATA
        JSR DRAW_FIELD
        JSR FLIP_SCREEN_BUFFERS

        LDA #$01
        STA COLOR
        JSR GEN_NEW_PIECE_COLOR
        LDA #6

- 
        JSR RUN_PIECE_STEP
        JMP -
        LDX #0
        LDY #0
        
        JSR SET_SCREEN
        
.DONE
        LDA TEMPA
        CLC
        ADC #1
        JMP -
        JSR WAIT_FOR_RASTER
     ;   JSR SET_EXTENDED_COLOR_MODE
     ;   JSR FUCK_SHIT_UP
     ;   JSR WAIT_FOR_KEY
        RTS


          
;-----------------------------------------------------------------------
;                                                   COPY_CUSTOM_CHARS
;-----------------------------------------------------------------------
!ZN COPY_CUSTOM_CHARS
COPY_CUSTOM_CHARS

          LDA #<CUSTOM_CHARACTER_DATA
          STA POINTER1_LO
          LDA #>CUSTOM_CHARACTER_DATA
          STA POINTER1_HI 

          LDA BUFFER_CHARACTER
          STA POINTER2_LO
          LDA BUFFER_CHARACTER+1
          STA POINTER2_HI

          LDX #$00
          LDY #$78
          JSR MEMCPY

          RTS

;-----------------------------------------------------------------------
;                                                   COPY_CUSTOM_SCREEN
;-----------------------------------------------------------------------
!ZN COPY_CUSTOM_SCREEN
COPY_CUSTOM_SCREEN

        LDA #<SCREEN1
        STA POINTER1_LO
        LDA #>SCREEN1
        STA POINTER1_HI 

        LDA BUFFER_SCREEN_FRONT
        STA POINTER2_LO
        LDA 1+BUFFER_SCREEN_FRONT
        STA POINTER2_HI

        LDX #$03
        LDY #$E8
        JSR MEMCPY

        LDA #<SCREEN1
        STA POINTER1_LO
        LDA #>SCREEN1
        STA POINTER1_HI 

        LDA BUFFER_SCREEN_BACK
        STA POINTER2_LO
        LDA 1+BUFFER_SCREEN_BACK
        STA POINTER2_HI

        LDX #$03
        LDY #$E8
        JSR MEMCPY

        LDA #<SCREEN1_COLOR
        STA POINTER1_LO
        LDA #>SCREEN1_COLOR
        STA POINTER1_HI

        LDA #<COLOR_MEMORY
        STA POINTER2_LO
        LDA #>COLOR_MEMORY
        STA POINTER2_HI
        
        LDX #$03
        LDY #$E8
        JSR MEMCPY
          
        RTS
          
;-----------------------------------------------------------------------
;                                                            SET_COLORS
;-----------------------------------------------------------------------
!ZN SET_COLORS
SET_COLORS
        
        LDA #$00
        STA $D020
        
        STA $D021

        LDA #7
        LDX #8
        LDY #9
        JSR SET_EXTENDED_COLORS
        RTS

;-----------------------------------------------------------------------
;                                                       Draw field
;-----------------------------------------------------------------------
; Draws the field to the current buffer
!ZN DRAW_FIELD
DRAW_FIELD

        LDA #<FIELD_DATA
        STA POINTER1_LO
        LDA #>FIELD_DATA        ; Get a pointer to the field data into
        STA POINTER1_HI         ; pointer1

        LDA BUFFER_SCREEN_CURRENT
        STA POINTER2_LO
        LDA 1+BUFFER_SCREEN_CURRENT     ; Get a pointer to the current screen
        STA POINTER2_HI                 ; buffer into pointer2

        CLC
        LDA POINTER2_LO
        ADC #<FIELD_OFFSET
        STA POINTER2_LO
        LDA POINTER2_HI         ; pointer 2 now points to
        ADC #$00                ; the curren screen buffer
        STA POINTER2_HI         ; plus FILED_OFFSET


        LDX #$14
        LDY #$09
        CLC
.YLOOP
        
        LDA (POINTER1),Y
        STA (POINTER2),Y
        
        DEY
        BNE .YLOOP
        
        LDA (POINTER1),Y
        STA (POINTER2),Y

        DEX
        BEQ .XDONE
        
        LDY #$09
        
        LDA POINTER2_LO
        ADC #$28
        STA POINTER2_LO
        BCC .NO_CARRY
        INC POINTER2_HI
        CLC
        
.NO_CARRY
        LDA POINTER1_LO
        ADC #$0A
        STA POINTER1_LO
        BCC .YLOOP
        INC POINTER1_HI
        CLC
        
        JMP .YLOOP

.XDONE        
        RTS

;-----------------------------------------------------------------------
;                                                  set field color data
;-----------------------------------------------------------------------
; A - Color used to fill the field
; Stores A in each byte for FIELD_COLOR_DATA
!ZN SET_FIELD_COLOR_DATA
SET_FIELD_COLOR_DATA

  ldy #<FIELD_COLOR_DATA
  sty POINTER1_LO
  ldy #>FIELD_COLOR_DATA  ; Get a pointer to the field color data into
  sty POINTER1_HI         ; pointer1
  
  ldy #199                ; Loop 199 chars
  
- sta (POINTER1),y        ; Stor color byte
  dey                     
  bne -                   ; Copy from 199 - 1, break at 0
  sta (POINTER1),y        ; Copy the last byte (index 0)
  rts                     ; exit

;-----------------------------------------------------------------------
;                                                       Set field data
;-----------------------------------------------------------------------
; A - The character to fill the field data with

; Set the entire field data fuffer to the same character, (the one in A)
!ZN SET_FIELD_DATA
SET_FIELD_DATA

  ldy #<FIELD_DATA
  sty POINTER1_LO
  ldy #>FIELD_DATA        ; Get a pointer to the
  sty POINTER1_HI         ; field data

  ldy #199                ; Fill 199 characters

- sta (POINTER1),Y        ; Store A to field data
  dey                     ; dec y
  bne -                   ; Done? no? Draw another one.
  sta (POINTER1),Y        ; Fill index 0.

  rts
  
;----------------------------------------------------------------------.
;                                              Set the joystick flags. |       
;-----------------------------------------------------------------------
; Test joystick states. Sets the following flags if the corrisponding  |
; joystick state is detected.                                          |
;   - FIRE_ACTIVE                                                      |
;   - JOY_LEFT                                                         |
;   - JOY_RIGHT                                                        |
;   - JOY_DOWN                                                         |
;----------------------------------------------------------------------'
!ZN SET_JOYSTICK_FLAGS
SET_JOYSTICK_FLAGS

; Reset the joystick flags
  lda #$00
  sta FIRE_ACTIVE         ; Reset the fire active flag.
  sta JOY_LEFT            ; Reset the JOY_LEFT flag.
  sta JOY_RIGHT           ; Reset the JOY_RIGHT flag.
  sta JOY_DOWN            ; Reset the JOY_DOWN flag.
  sta JOY_UP

.FIRE
  lda $DC01
  bit 5+BITS
  bne .ZERO_TIMER
  lda FIRE_LOCKOUT_TIMER
  beq .ACTIVATE
  dec FIRE_LOCKOUT_TIMER
  jmp .UP
  
.ACTIVATE
  inc FIRE_ACTIVE
  ldy #FIRE_LOCKOUT_TIMER_RESET
  sty FIRE_LOCKOUT_TIMER
  jmp .UP
  
.ZERO_TIMER
  lda #$00
  sta FIRE_LOCKOUT_TIMER

.UP
  lda $DC01
  bit 1+BITS
  bne .DOWN
  inc JOY_UP      

.DOWN
  bit 2+BITS
  bne .LEFT
  inc JOY_DOWN

.LEFT
  bit 3+BITS
  bne .RIGHT
  inc JOY_LEFT

.RIGHT
  bit 4+BITS
  bne .JOY_EXIT
  inc JOY_RIGHT
        
.JOY_EXIT
  rts


;-----------------------------------------------------------------------
;                                                   Clip piece location
;-----------------------------------------------------------------------
; Force CURRENT_PIECE_LOCATION to be in a position such that the current
; piece is inside the board. 

; To do this, we have to take into account how the pieces are defined
; and how that piece data is related to the CURRENT_PIECE_LOCATION.

; First, CURRENT_PIECE_LOCATION represents the upper left piece part of
; the piece. This is used to make sure that the piece is not above or to the
; right of the field.

; To check the piece against the right and bottom of the field, we have to
; know the size of the piece. We need the width of the piece to check the
; piece against the right side of the field, and we need the height of the
; piece to check the piece against the bottom of the field.
; ie make sure that:
; CURRENT_LOCATION + PIECE_WIDTH < RIGHT SIDE OF FIELD.
; CURRENT_LOCATION + PIECE_HEIGHT < BOTTOM SIDE OF FIELD.


; CURRENT_PIECE_PTR points to the PIECE_DATA representing the current piece. 
; This data defines the locations of the other 3 piece parts relative to 
; CURRENT_PIECE_LOCATION.

; More importantly, the fifth byte of the piece data is the width-1 of the piece
; and the sixth byte is the height-11

!ZN CLIP_PIECE_LOCATION
CLIP_PIECE_LOCATION
        
        SEC
        LDA #FIELD_START_X
        LDY #$04
        SBC (CURRT_PIECE_PTR),Y
        CMP CURRENT_PIECE_LOCATION_X
        BCC .XEND
        STA CURRENT_PIECE_LOCATION_X
        JMP .YSTART

.XEND
; First, get the width of the piece from the fourth byte of the piece data 
; pointed to by the current piece ptr
        LDA #FIELD_END_X                      ; Get field end
        LDY #$06
        CLC
        ADC (CURRT_PIECE_PTR),Y               ; Subtract piece width
        
        CMP CURRENT_PIECE_LOCATION_X          ; Compare current location
        BCS .YSTART
        STA CURRENT_PIECE_LOCATION_X

.YSTART 
        LDA #FIELD_START_Y
        LDY #05
        SEC
        SBC (CURRT_PIECE_PTR),Y
        CMP CURRENT_PIECE_LOCATION_Y
        BCC .YEND
        STA CURRENT_PIECE_LOCATION_Y
        RTS

.YEND        
        LDA #FIELD_END_Y                        ; Get field bottom
        LDY #$07
        CLC
        ADC (CURRT_PIECE_PTR),Y                 ; Subtract piece h-1
        
        CMP CURRENT_PIECE_LOCATION_Y        ; Compare current location
        BCS .DONE
        STA CURRENT_PIECE_LOCATION_Y

.DONE
        RTS

;-----------------------------------------------------------------------
;                                                       Run piece step
;-----------------------------------------------------------------------
!ZN RUN_PIECE_STEP
RUN_PIECE_STEP
        
        jsr RUN_DROP_LOGIC
        JSR SET_JOYSTICK_FLAGS
        jsr UPDATE_CURRENT_PIECE_PTR
        jsr UPDATE_CURRENT_PIECE_LOCATION
        JSR CLIP_PIECE_LOCATION
        
        ldx CURRENT_PIECE_LOCATION_X
        ldy CURRENT_PIECE_LOCATION_Y
        jsr DETECT_COLLISION
        ldx #$00
        lda COLLISION_DETECTED
        beq +
        ldx #$01
+       stx $d021

        LDA PREV_PIECE_PTR_LO
        LDY PREV_PIECE_PTR_HI
        STA GEN_PIECE_PTR_LO
        STY GEN_PIECE_PTR_HI
        
        ;LDX CURRENT_PIECE_LOCATION_X
        ;LDY CURRENT_PIECE_LOCATION_Y
        ;JSR STORE_PIECE_COLOR

.DRAW
        LDA #1
        STA $D020
        JSR DRAW_FIELD
        LDA #0
        STA $D020
        ldx #14

        LDX #RASTER_TIMER_RESET

-       JSR WAIT_FOR_RASTER        
        DEX
        BNE -

        ;LDA MOVING_FLAG
        ;BEQ .NO_COLOR_RESTORE

        LDX PREV_PIECE_LOCATION_X
        LDY PREV_PIECE_LOCATION_Y
        JSR RESTORE_PIECE_COLOR
        
.NO_COLOR_RESTORE        

        LDA CURRT_PIECE_PTR_LO
        STA GEN_PIECE_PTR_LO
        LDA CURRT_PIECE_PTR_HI
        STA GEN_PIECE_PTR_HI
        
        lda CURRENT_PIECE_COLOR
        STA COLOR
        LDA #02
        LDX CURRENT_PIECE_LOCATION_X
        LDY CURRENT_PIECE_LOCATION_Y
        JSR DRAW_PIECE
        
        JSR FLIP_SCREEN_BUFFERS     
        JSR UPDATE_PREV_PIECE_LOCATION
        JSR UPDATE_PREV_PIECE_PTR

        RTS

;-----------------------------------------------------------------------.
;                                     Update the current piece rotation |
;------------------------------------------------------------------------
; Check FIRE_ACTIVE, if the flag is set, CURRT_PIECE_PTR is adjusted to |
; point to the next rotation frame of the current piece.                |
;-----------------------------------------------------------------------'
!ZN UPDATE_CURRENT_PIECE_PTR
UPDATE_CURRENT_PIECE_PTR

        LDA FIRE_ACTIVE
        BEQ .END_UPDATE
        
        jsr GEN_NEW_PIECE_COLOR
        
        Jsr COPY_PIECE_TO_FIELD
        jsr COPY_PIECE_COLOR_TO_FIELD_COLOR_DATA
        
        
        CLC
        LDA CURRT_PIECE_PTR_LO
        ADC #(NUMBER_OF_PIECES*PIECE_DATA_WIDTH)               ; ADD 49 BYTES (7 pieces * 7 bytes/piece)
        BCS .RESET
        CLC
        CMP #<PIECE_DATA_END
        BCS .RESET
        STA CURRT_PIECE_PTR_LO
        JMP .END_UPDATE
        
.RESET
        SEC
        LDA CURRT_PIECE_PTR_LO
        SBC #(PIECE_DATA_BLOCK_WIDTH*3) ;($93)
        STA CURRT_PIECE_PTR_LO


.END_UPDATE
        RTS
        
;-----------------------------------------------------------------------.
;                                     Update the current piece location |
;------------------------------------------------------------------------
; Analize the state of the JOY_XXXX flags combined with collision       | 
; using the DETECT_COLLISION routine to update the current location of  |
; the piece.                                                            |
;-----------------------------------------------------------------------'

!zn UPDATE_CURRENT_PIECE_LOCATION
UPDATE_CURRENT_PIECE_LOCATION

  lda DROP_FLAG
  beq +
  jsr COLLIDE_DOWN
  bne ++
  inc CURRENT_PIECE_LOCATION_Y
  jmp ++
  
+ lda JOY_DOWN
  beq ++
  jsr COLLIDE_DOWN
  bne ++
  inc CURRENT_PIECE_LOCATION_Y
  
++lda JOY_UP
  beq +
  dec CURRENT_PIECE_LOCATION_Y
  
+ lda JOY_LEFT
  beq +
  jsr COLLIDE_LEFT
  bne +
  dec CURRENT_PIECE_LOCATION_X
  
+ lda JOY_RIGHT
  beq +
  jsr COLLIDE_RIGHT
  bne +
  inc CURRENT_PIECE_LOCATION_X
  
+ rts


;-----------------------------------------------------------------------.
;                                            Update prev piece location |
;------------------------------------------------------------------------
; Sets PREVIOUS_PIECE_LOCATION equal to CURRENT_PIECE_LOCATION          |
;-----------------------------------------------------------------------'

!ZN UPDATE_PREV_PIECE_LOCATION
UPDATE_PREV_PIECE_LOCATION

  lda CURRENT_PIECE_LOCATION_X
  sta PREV_PIECE_LOCATION_X
  lda CURRENT_PIECE_LOCATION_Y
  sta PREV_PIECE_LOCATION_Y
  rts

;-----------------------------------------------------------------------.
;                                                 Update prev piece ptr |
;------------------------------------------------------------------------
; Sets PREVIOUS_PIECE_LOCATION equal to CURRENT_PIECE_LOCATION          |
;-----------------------------------------------------------------------'
!ZN UPDATE_PREV_PIECE_PTR
UPDATE_PREV_PIECE_PTR

  lda CURRT_PIECE_PTR_LO
  sta PREV_PIECE_PTR_LO
  lda CURRT_PIECE_PTR_HI
  sta PREV_PIECE_PTR_HI

  rts
        

;-----------------------------------------------------------------------.
;                                                            Draw piece |
;------------------------------------------------------------------------
; Draws the piece pointed to by GEN_PIECE_POINTER to the current screen |
; buffer.                                                               |
;------------------------------------------------------------------------
; A = Character code used to draw the piece                             |
; X = X location of the piece                                           |
; Y = Y location of the piece                                           |
; GEN_PIECE_PTR = pointer the the piece data used to draw the piece.    |
; COLOR is used to color the peice                                      |
;-----------------------------------------------------------------------'

!ZN DRAW_PIECE
DRAW_PIECE

        STA CHARACTER_CODE              ; Store the character code used to
                                        ; draw the piece.

        STX TEMPX                       ; Save the x piece location

        TYA                          ; <-- xfer the y location to A
        STA FAC1                     ;   |
        LDA #40                      ;   |
        STA FAC2                     ;   |
        JSR MUL8                     ;   |  -> Calculate the Y offset of the first
        ; A - Y Offset hi            ;   |     part of the piece and store it in
        ; X - Y Offset lo            ;   |     TEMP. The same offset is used for
                                     ;   |     the screen buffer and the color.
        STX OFFSET_LO                ;   |
        STA OFFSET_HI                ; <--
                             
        CLC
        LDA OFFSET_LO
        ADC BUFFER_SCREEN_CURRENT
        STA POINTER2_LO             ; Calculate LO byte.
        LDA OFFSET_HI                         ; Pull back the hi byte of OFFSET
        ADC 1+BUFFER_SCREEN_CURRENT ; Add the temp offset to the current screen 
        STA POINTER2_HI             ; buffer. Store in pointer2  

        
        LDA #<COLOR_MEMORY
        ADC OFFSET_LO
        STA POINTER3_LO
        LDA #>COLOR_MEMORY
        ADC OFFSET_HI                   ; Add the temp offset to the coller
        STA POINTER3_HI                 ; buffer, store in pointer3
        
        LDA TEMPX
        LDY #$00
        ADC (GEN_PIECE_PTR),Y
        TAY
        LDA CHARACTER_CODE
        STA (POINTER2_LO),Y
        LDA COLOR
        STA (POINTER3_LO),Y
        
        LDA TEMPX
        LDY #$01
        ADC (GEN_PIECE_PTR),Y
        TAY
        LDA CHARACTER_CODE
        STA (POINTER2_LO),Y
        LDA COLOR
        STA (POINTER3_LO),Y

        LDY #$02
        LDA (GEN_PIECE_PTR),Y
        ADC TEMPX
        TAY
        LDA CHARACTER_CODE
        STA (POINTER2_LO),Y
        LDA COLOR
        STA (POINTER3_LO),Y

        LDY #$03
        LDA (GEN_PIECE_PTR),Y
        ADC TEMPX
        TAY
        LDA CHARACTER_CODE
        STA (POINTER2_LO),Y
        LDA COLOR
        STA (POINTER3_LO),Y


        RTS

        
;-----------------------------------------------------------------------.
;                                                   Restore peice color |
;------------------------------------------------------------------------
; Restores the color data behind a piece location from the color data   |
; in FIELD_COLOR_DATA, copying it to COLOR_MEMORY at the location       |
; specified by the X,Y inputs.                                          |
;------------------------------------------------------------------------
; GEN_PIECE_PTR - points to the peice data structure used to replace    |
;               - the color data. The piece data structure is also      |
;               - is also needed so that we can reference bytes 4-9     |
;               - which are the offset bytes used when the peice is near|
;               - The edge of the screen.
; X - X location of the piece                                           |
; Y - Y location of the piece                                           |
;-----------------------------------------------------------------------'

!ZN RESTORE_PIECE_COLOR
RESTORE_PIECE_COLOR

; We need to convert the x,y position to two different offsets, we need the
; offset in screen space and the offset in field space.

; First we store x,y so we can get it back.
  stx TEMPX
  sty TEMPY
  
; NEXT, convert y to a screen space offset by multiplying by 40
  sty FAC1
  lda #$28
  sta FAC2
  jsr MUL8
  
; We are going to store the screen space offset in POINTER1
  stx POINTER1_LO
  sta POINTER1_HI
  
; Add the x offset.
  clc
  txa
  adc TEMPX
  sta POINTER1_LO
  bcc +
  inc POINTER1_HI
+

------------------------------------
; NEXT convert x,y to field space and store it in pointer2

; First, convert Y
  lda TEMPY   
  sec
; The field is offset by 3 but because of the offset scheme, the
; upper left hand corner can be 1 row above and one row to the 
; left of the upper left corner of the field. We can't use negative
; values so we need to subtract 2 and 12 instead of 3 and 13, and
; then we have to adjust the field color data ptr to compensate.
  sbc #$02  ; The field is offset by 2 so subtract that first
  sta FAC1

  lda #$0A
  sta FAC2
  jsr MUL8  ; multiply by 10, the width of the field
  stx POINTER2_LO
  sta POINTER2_HI
  
; convert x to field space
  lda TEMPX
  sec
; The field is offset by 3 but because of the offset scheme, the
; upper left hand corner can be 1 row above and one row to the 
; left of the upper left corner of the field. We can't use negative
; values so we need to subtract 2 and 12 instead of 3 and 13, and
; then we have to adjust the field color data ptr to compensate.
  sbc #$0c        ; Subtract 12 to move from screen space to field space.
  sta TEMPX
  
; add x to y to get the final offset
  clc 
  lda POINTER2_LO
  adc TEMPX
  sta POINTER2_LO
  bcc +
  inc POINTER2_HI
+
  
; Add POINTER1 and COLOR_MEMORY to get the final screenspace offset
  clc
  lda POINTER1_LO
  adc #<COLOR_MEMORY
  sta POINTER1_LO
  lda POINTER1_HI
  adc #>COLOR_MEMORY
  sta POINTER1_HI
  
; Add POINTER2 and FIELD_COLOR_DATA to get the final field offset
; The field is offset by 3 but because of the offset scheme, the
; upper left hand corner can be 1 row above and one row to the 
; left of the upper left corner of the field. We can't use negative
; values so we need to subtract 2 and 12 instead of 3 and 13, and
; then we have to adjust the field color data ptr to compensate.
  clc
  lda POINTER2_LO
  adc #<FIELD_COLOR_DATA-11
  sta POINTER2_LO
  lda POINTER2_HI
  adc #>FIELD_COLOR_DATA-1
  sta POINTER2_HI
        
; Replace the color behind the first piece part 
; POINTER1_LO + GEN_PIECE_PTR[0] + X = COLOR_DATA_PTR[0]
  clc
  ldy #$00
  lda (GEN_PIECE_PTR),Y   ; Get the offset of the first part.
  sta TEMPA               ; Save it for when we stor in screen space
  JSR CVTY4010            ; Convert to field space
  TAY                     ; Set as index
  LDA (POINTER2),Y        ; Get the color in pos1 of the color data
  ldy TEMPA               ; Reload the screen space off set
  STA (POINTER1),Y        ; Set the color

; Replace the color behind the second piece part 
; POINTER1_LO + X - GEN_PIECE_PTR[1] = COLOR_DATA_PTR[1]          
  clc
  ldy #$01
  lda (GEN_PIECE_PTR),Y   ; Get the offset of the first part.
  sta TEMPA               ; Save it for when we stor in screen space
  JSR CVTY4010            ; Convert to field space
  TAY                     ; Set as index
  LDA (POINTER2),Y        ; Get the color in pos1 of the color data
  ldy TEMPA               ; Reload the screen space off set
  STA (POINTER1),Y        ; Set the color

; Replace the color behind the third piece part 
; POINTER1_LO + GEN_PIECE_PTR[2] + X = COLOR_DATA_PTR[2]
  clc
  ldy #$02
  lda (GEN_PIECE_PTR),Y   ; Get the offset of the first part.
  sta TEMPA               ; Save it for when we stor in screen space
  JSR CVTY4010            ; Convert to field space
  TAY                     ; Set as index
  LDA (POINTER2),Y        ; Get the color in pos1 of the color data
  ldy TEMPA               ; Reload the screen space off set
  STA (POINTER1),Y        ; Set the color

; Replace the color behind the fourth piece part 
; POINTER1_LO + GEN_PIECE_PTR[3] + X = COLOR_DATA_PTR[3]       
  clc
  ldy #$03
  lda (GEN_PIECE_PTR),Y   ; Get the offset of the first part.
  sta TEMPA               ; Save it for when we stor in screen space
  JSR CVTY4010            ; Convert to field space
  TAY                     ; Set as index
  LDA (POINTER2),Y        ; Get the color in pos1 of the color data
  ldy TEMPA               ; Reload the screen space off set
  STA (POINTER1),Y        ; Set the color
        
        RTS

;-----------------------------------------------------------------------.
;                                     Generate the color of a new piece |
;------------------------------------------------------------------------
; Get a random number between 1 and 15 (exclude $00 and $0B, the piece  |
; border color and the background. Store THE new color in               |
; CURRENT_PIECE_COLOR.                                                  |
;-----------------------------------------------------------------------'

!ZN GEN_NEW_PIECE_COLOR
GEN_NEW_PIECE_COLOR

; Set the max val and the mask

- lda #$0F    ; max val
  ldx #$0F    ; Mask
  
  jsr RND     ; RND sub routine
  cmp $00
  beq -       ; Re-roll if black
  cmp $0B
  beq -       ; Re-roll if grey (background color)
  
  sta CURRENT_PIECE_COLOR
  rts
        
;-----------------------------------------------------------------------.
;               Copy the color of the current piece to FIELD_COLOR_DATA |
;------------------------------------------------------------------------
; Copy the color of the current piece to color memory.(FILED_COLOR_DATA)|
; CURRENT_PIECE_PTR should hold the address of the piece shape to be    |
; used to store the color.                                              |
; CURRENT_PIECE_LOCATION is used as the position of the piece.          |
; CURRENT_PIECE_COLOR is used as the color to store.                    |
;-----------------------------------------------------------------------'

!ZN COPY_PIECE_COLOR_TO_FIELD_COLOR_DATA
COPY_PIECE_COLOR_TO_FIELD_COLOR_DATA

; Calculate the offset of the current piece from the start of the field

  sec
  
  lda CURRENT_PIECE_LOCATION_X
  sbc #FIELD_START_X-1
  sta TEMPX
  
  lda CURRENT_PIECE_LOCATION_Y
  sec
  sbc #FIELD_START_Y-1
  
  sta FAC1
  lda #FIELD_DATA_WIDTH
  sta FAC2
  jsr MUL8
  txa
  clc
  adc TEMPX
  
  adc #<FIELD_COLOR_DATA ;not needed, aligned
  sta POINTER1_LO
  lda #>FIELD_COLOR_DATA
  sta POINTER1_HI
  
  sec
  lda POINTER1_LO
  SBC #11
  STA POINTER1_LO
  LDA POINTER1_HI
  SBC #00
  STA POINTER1_HI
  
  clc
  ldy #$00
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda CURRENT_PIECE_COLOR
  sta (POINTER1),Y
  
  clc
  ldy #$01
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda CURRENT_PIECE_COLOR
  sta (POINTER1),Y
  
  clc
  ldy #$02
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda CURRENT_PIECE_COLOR
  sta (POINTER1),Y
  
  clc
  ldy #$03
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda CURRENT_PIECE_COLOR
  sta (POINTER1),Y
  
  rts
  
        
;-----------------------------------------------------------------------.
;                                     Copy current piece to field data. |
;------------------------------------------------------------------------
; Copies the piece data pointed to by CURRT_PIECE_PTR to FIELD_DATA.    |
;-----------------------------------------------------------------------'

!ZN COPY_PIECE_TO_FIELD
COPY_PIECE_TO_FIELD

; Calculate the offset of the current piece from the start of the field

  sec
  
  lda CURRENT_PIECE_LOCATION_X
  sbc #FIELD_START_X-1
  sta TEMPX
  
  lda CURRENT_PIECE_LOCATION_Y
  sec
  sbc #FIELD_START_Y-1
  
  sta FAC1
  lda #FIELD_DATA_WIDTH
  sta FAC2
  jsr MUL8
  txa
  clc
  adc TEMPX
  
  adc #<FIELD_DATA ;not needed, aligned
  sta POINTER1_LO
  lda #>FIELD_DATA
  sta POINTER1_HI
  
  sec
  lda POINTER1_LO
  SBC #11
  STA POINTER1_LO
  LDA POINTER1_HI
  SBC #00
  STA POINTER1_HI
  
  clc
  ldy #$00
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda #$01
  sta (POINTER1),Y
  
  clc
  ldy #$01
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda #$01
  sta (POINTER1),Y
  
  clc
  ldy #$02
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda #$01
  sta (POINTER1),Y
  
  clc
  ldy #$03
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda #$01
  sta (POINTER1),Y
  
  rts
  
;-----------------------------------------------------------------------.
;                                                      Detect collision |
;------------------------------------------------------------------------
; Detects a collision between the current piece (indicated by           |
; CURRT_PIECE_PTR). There is no indication of where the collision took  |
; place.                                                                |
; If a collision is detected, COLLISION_DETECTED is set to true,        |
; otherwise COLLISION_FLAG will be false.                               |
; X - The x-position for which to test the piece.                       |
; Y - The y-position for which to test the piece.                       |
;-----------------------------------------------------------------------'

!ZN DETECT_COLLISION
DETECT_COLLISION

; Reset COLLISION_DETECTED
  lda #$00
  sta COLLISION_DETECTED
  
; Store the x,y location inputs
  stx TEMPX
  sty TEMPY

; Calculate the offset of the current piece from the start of the field

  sec
  lda TEMPX
  sbc #FIELD_START_X-1
  sta TEMPX
  
  sec
  lda TEMPY
  sbc #FIELD_START_Y-1
  
  sta FAC1
  lda #FIELD_DATA_WIDTH
  sta FAC2
  jsr MUL8
  txa
  clc
  adc TEMPX
  
  adc #<FIELD_DATA ;not needed, aligned
  sta POINTER1_LO
  lda #>FIELD_DATA
  sta POINTER1_HI
  
  sec
  lda POINTER1_LO
  SBC #11
  STA POINTER1_LO
  LDA POINTER1_HI
  SBC #00
  STA POINTER1_HI
  
  clc
  ldy #$00
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda (POINTER1),Y
  beq +
  inc COLLISION_DETECTED
  rts
  
+ clc
  ldy #$01
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda (POINTER1),Y
  beq +
  inc COLLISION_DETECTED
  rts
  
+ clc
  ldy #$02
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda (POINTER1),Y
  beq +
  inc COLLISION_DETECTED
  rts
  
+ clc
  ldy #$03
  lda (CURRT_PIECE_PTR),Y
  jsr CVTY4010
  tay
  lda (POINTER1),Y
  beq +
  inc COLLISION_DETECTED
  rts
  
+ rts
  
;-----------------------------------------------------------------------.
;                    Convert a fourty byte Y offset to a 10 by y offset |
;------------------------------------------------------------------------
; Converts a 40 byte Y offset (a y offset in screen space) to a 10 byte |
; y offset (a Y offset in field space)                                  |
;-----------------------------------------------------------------------'
; A - The byte to be converted.

!zn CVTY4010
CVTY4010
  
  cmp #$77
  bcc +
  sec
  sbc #90
  jmp ++
  
+ cmp #$50
  bcc +
  sec
  sbc #60
  jmp ++
  
+ cmp #$27
  bcc ++
  sec
  sbc #30
  
++ rts

;-----------------------------------------------------------------------.
;                                                   Increase drop speed |
;------------------------------------------------------------------------
; Pull the next byte in PIECE_DROP_TIMER_RESET_VECTOR. Check the positon|
; in the vector to make sure that we don't pass the end of the vector   |
; When the player reaches the end of the vector the pieces won't drop   |
; any faster.                                                           |
;------------------------------------------------------------------------

!zn INCREASE_DROP_SPEED
INCREASE_DROP_SPEED

  lda PIECE_DROP_TIMER_RESET_VECTOR_POSITION
  cmp #PIECE_DROP_TIMER_RESET_VECTOR_LENGTH-1
  bne +
  rts
+ inc PIECE_DROP_TIMER_RESET_VECTOR_POSITION
  ldx PIECE_DROP_TIMER_RESET_VECTOR_POSITION
  lda PIECE_DROP_TIMER_RESET_VECTOR,X
  sta PIECE_DROP_TIMER_RESET
  rts

;-----------------------------------------------------------------------.
;                                                            Drop piece |
;------------------------------------------------------------------------
; Run the logic that drops the piece.                                   |
; - Lower the current value of the timer,                               |
;     If the timer has not gone off return                              |
;     Otherwise, reset the time and set the DROP_FLAG                   |
;------------------------------------------------------------------------

!zn RUN_DROP_LOGIC
RUN_DROP_LOGIC

  lda #$00
  sta DROP_FLAG

  dec PIECE_DROP_TIMER
  beq +
  rts
+ lda PIECE_DROP_TIMER_RESET
  sta PIECE_DROP_TIMER
  inc DROP_FLAG
  rts
  
;-----------------------------------------------------------------------.
;                                                          Collide Left |
;------------------------------------------------------------------------
; Determines if there would be a collision if the current piece were to |
; move to the left.                                                     |
; A - 1 if a collision is detected, 0 otherwise.                        |
;-----------------------------------------------------------------------'
!zn COLLIDE_LEFT
COLLIDE_LEFT

  lda CURRENT_PIECE_LOCATION_X
  sec
  sbc #$01
  tax
  ldy CURRENT_PIECE_LOCATION_Y
  jsr DETECT_COLLISION
  lda COLLISION_DETECTED
  rts
  

;-----------------------------------------------------------------------.
;                                                         Collide Right |
;------------------------------------------------------------------------
; Determines if there would be a collision if the current piece were to |
; move to the right.                                                    |
; A - 1 if a collision is detected, 0 otherwise.                        |
;-----------------------------------------------------------------------'
!zn COLLIDE_RIGHT
COLLIDE_RIGHT

  lda CURRENT_PIECE_LOCATION_X
  clc
  adc #$01
  tax
  ldy CURRENT_PIECE_LOCATION_Y
  jsr DETECT_COLLISION
  lda COLLISION_DETECTED
  rts

;-----------------------------------------------------------------------.
;                                                          Collide Down |
;------------------------------------------------------------------------
; Determines if there would be a collision if the current piece were to |
; move down.                                                            |
; A - 1 if a collision is detected, 0 otherwise.                        |
;-----------------------------------------------------------------------'

!zn COLLIDE_DOWN
COLLIDE_DOWN

  lda CURRENT_PIECE_LOCATION_Y
  clc
  adc #$01
  tay
  ldx CURRENT_PIECE_LOCATION_X
  jsr DETECT_COLLISION
  lda COLLISION_DETECTED
  rts
  
;***********************************************************************
;                                                        CHARACTER DATA
;-----------------------------------------------------------------------

CUSTOM_CHARACTER_DATA
        !BYTE    $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ; CHARACTER 0
        !BYTE    $00,$7E,$42,$5A,$5A,$42,$7E,$00 ; CHARACTER 1
        !BYTE    $00,$7E,$42,$5A,$5A,$42,$7E,$00 ; CHARACTER 2
        !BYTE    $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ; CHARACTER 3
        !BYTE    $3E,$1F,$0B,$0D,$07,$01,$00,$00 ; CHARACTER 4
        !BYTE    $00,$00,$80,$C0,$B0,$D0,$F8,$7C ; CHARACTER 5
        !BYTE    $00,$00,$01,$03,$0F,$05,$1F,$3E ; CHARACTER 6
        !BYTE    $7C,$E8,$B0,$D0,$C0,$80,$00,$00 ; CHARACTER 7
        !BYTE    $00,$00,$81,$EF,$F7,$FF,$FF,$00 ; CHARACTER 8
        !BYTE    $3E,$1E,$1E,$16,$0E,$1E,$1E,$3E ; CHARACTER 9
        !BYTE    $00,$FF,$FF,$EF,$F7,$81,$00,$00 ; CHARACTER 10
        !BYTE    $7C,$78,$78,$70,$68,$78,$78,$7C ; CHARACTER 11
        !BYTE    $7E,$7E,$7E,$7E,$7E,$7E,$7E,$7E ; CHARACTER 12
        !BYTE    $3E,$5F,$5B,$65,$7B,$69,$7C,$7E ; CHARACTER 13
        !BYTE    $7E,$74,$65,$63,$4F,$05,$1F,$3E ; CHARACTER 14

;-----------------------------------------------------------------------
;                                                           PIECE DATA
;-----------------------------------------------------------------------

CURRENT_PIECE_LOCATION_X
        !BYTE $0D
CURRENT_PIECE_LOCATION_Y
        !BYTE $03

PREV_PIECE_LOCATION_X
        !BYTE $0D
PREV_PIECE_LOCATION_Y
        !BYTE $03


; Piece data is encoded as four positions within a 4x4 grid.
; Since none of the pieces take up the entire space, when the
; piece is clipped, the blank area needs to be adjusted for.
; The 4th and 5th bytes are the amout of empty space on the
; left and top side, the 6th and 7th bytes are the amout
; of empty space on the right and top sides
!ALIGN 255,0
PIECE_DATA
            ;|00| |01| |02| |03|   |EL| |ET| |ER| |EB|
        !BYTE $28, $29, $2A, $2B,  $00, $01, $00, $02
        !BYTE $28, $29, $51, $52,  $00, $01, $01, $01
        !BYTE $29, $2A, $50, $51,  $00, $01, $01, $01
        !BYTE $00, $01, $28, $29,  $00, $00, $02, $02
        !BYTE $28, $29, $2A, $52,  $00, $01, $01, $01
        !BYTE $28, $29, $2A, $50,  $00, $01, $01, $01
        !BYTE $01, $28, $29, $2A,  $00, $00, $01, $02

PIECE_DATA_LEFT
        !BYTE $01, $29, $51, $79,  $01, $00, $02, $00
        !BYTE $01, $28, $29, $50,  $00, $00, $02, $01
        !BYTE $00, $28, $29, $51,  $00, $00, $02, $01
        !BYTE $00, $01, $28, $29,  $00, $00, $02, $02
        !BYTE $01, $29, $50, $51,  $00, $00, $02, $01
        !BYTE $00, $01, $29, $51,  $00, $00, $02, $01
        !BYTE $01, $29, $2A, $51,  $01, $00, $01, $01

PIECE_DATA_UP
        !BYTE $28, $29, $2A, $2B,  $00, $01, $00, $02
        !BYTE $28, $29, $51, $52,  $00, $01, $01, $01
        !BYTE $29, $2A, $50, $51,  $00, $01, $01, $01
        !BYTE $00, $01, $28, $29,  $00, $00, $02, $02
        !BYTE $00, $28, $29, $2A,  $00, $00, $01, $02
        !BYTE $02, $28, $29, $2A,  $00, $00, $01, $02
        !BYTE $28, $29, $2A, $51,  $00, $01, $01, $01

PIECE_DATA_RIGHT
        !BYTE $01, $29, $51, $79,  $01, $00, $02, $00
        !BYTE $01, $28, $29, $50,  $00, $00, $02, $01
        !BYTE $00, $28, $29, $51,  $00, $00, $02, $01
        !BYTE $00, $01, $28, $29,  $00, $00, $02, $02
        !BYTE $00, $01, $28, $50,  $00, $00, $02, $01
        !BYTE $01, $29, $51, $52,  $01, $00, $01, $01
        !BYTE $01, $28, $29, $51,  $00, $00, $02, $01
PIECE_DATA_END

; used to store the color data under the current piece as it moves around
STORED_COLOR
        !BYTE $00, $00, $00, $00

; One byte of color data used to pass a color between routines
COLOR
        !BYTE $00
        
; Stores the color of the current piece.
CURRENT_PIECE_COLOR
        !BYTE $01

; Used to pass character codes to sub routines.
CHARACTER_CODE
        !BYTE $00

TEMP_LO
        !BYTE $00
TEMP_HI
        !BYTE $00
        
; Set or unset by DETECT_COLLISION. If true, the current piece/ piece location
; is colliding with the backgroun data located in FIELD_DATA.
COLLISION_DETECTED
        !BYTE $00

; Cleared at the start of SET_JOYSTICK_FLAGS, then set by SET_JOYSTICK_FLAGS 
; if the corrisponding joystick state is present
FIRE_ACTIVE        !BYTE $00
JOY_LEFT           !BYTE $00
JOY_RIGHT          !BYTE $00
JOY_DOWN           !BYTE $00
JOY_UP             !BYTE $00

; Set by the RUN_DROP_LOGIC routine when the PIECE_DROP_TIMER runs out. 
; Analyzed by the UPDATE_CURRENT_PIECE_LOCATION and forces the piece to move
; down if the flag is set.
DROP_FLAG          !BYTE $00

; The FIRE_LOCKOOUT_TIMER is a debounce timer that prevents the piece from
; rotating to quickly.
FIRE_LOCKOUT_TIMER !BYTE $00

; The number of raster resets before the piece drops.
; The PIECE_DROP_TIMER is used to count the raster timer resets between
; forced drops of the piece. As the level goes up the timer is reset to
; a lower and lower time. The reset values for each level are stored in
; PIECE_DROP_TIMER_RESET_VECTOR.
; PIECE_DROP_TIMER_RESET_VECTOR_POSION is the current position in this
; vector.
PIECE_DROP_TIMER                       !BYTE 15
PIECE_DROP_TIMER_RESET                 !BYTE 15
PIECE_DROP_TIMER_RESET_VECTOR_POSITION !BYTE 0
PIECE_DROP_TIMER_RESET_VECTOR          !BYTE 15, 18, 16, 14, 12,  10, 9, 8, 7, 5


;-----------------------------------------------------------------------
;                                                       Timer Variables
;-----------------------------------------------------------------------
; These variables hold the current value of various timers.
        
;-----------------------------------------------------------------------
;                                                                 MISC.
;-----------------------------------------------------------------------
!ALIGN 255,0
SCREEN1
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$06,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$05,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0D,$08,$08,$08,$08,$05,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0C,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0C,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0C,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0C,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$0A,$0A,$0A,$0A,$07,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$04,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$07,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  !BYTE  $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

!ALIGN 255,0
SCREEN1_COLOR
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

!ALIGN 255,0
FIELD_DATA
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00

        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00

        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00

        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        
!ALIGN 255,0
FIELD_COLOR_DATA
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00

        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00

        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00

        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00
        !BYTE    $00,$00,$00,$00,$00, $00,$05,$00,$00,$00

OFFSET_LO
        !BYTE $00
OFFSET_HI
        !BYTE $00