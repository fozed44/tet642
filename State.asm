;-----------------------------------------------------------------------.
;                                                             State.asm |
;-----------------------------------------------------------------------'
; Variables and routines to implement the state machine

;-----------------------------------------------------------------------.
;                                                       Possible states |
;-----------------------------------------------------------------------'

; Normal state. The player is controlling the falling piece.
STATE_NORMAL = $00

; Line clearing animation is underway
STATE_LINE_ANIMATION = $01

*=$7000

; Holds the current state. Do not access this variable directly.
CURRENT_STATE   !BYTE $00

;-----------------------------------------------------------------------.
;                                                 Get the current state |
;------------------------------------------------------------------------
; A - The current state is returned in A                                |
;-----------------------------------------------------------------------'
!zn GET_STATE
GET_STATE

  lda CURRENT_STATE
  rts
  
;-----------------------------------------------------------------------.
;                                                 Set the current state |
;------------------------------------------------------------------------
; A - The state                                                         |
;-----------------------------------------------------------------------'
!zn SET_STATE
SET_STATE

  sta CURRENT_STATE
  rts
  
;-----------------------------------------------------------------------.
;                                                    Move to next state |
;------------------------------------------------------------------------
; Move to the next state based on the current state.                    |
; A - Returns the new state.                                            |
;-----------------------------------------------------------------------'
!zn ADVANCE_STATE
ADVANCE_STATE

; If the current state is STATE_LINE_ANIMATION then the next state should be
; STATE_NORMAL
  lda CURRENT_STATE
  cmp #STATE_LINE_ANIMATION
  bne +
  lda #STATE_NORMAL
  rts
  
+ lda CURRENT_STATE
  rts