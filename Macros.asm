

;--------------------------------------------------------------------------
;                                                  Macros for common tasks
;--------------------------------------------------------------------------




;--------------------------------------------------------------------------
;                                                                     POKE
;--------------------------------------------------------------------------

defm    POKE
        LDA #/2
        STA /1
endm

;--------------------------------------------------------------------------
;                                                                     DOKE
;--------------------------------------------------------------------------

defm    DOKE
        
        LDA #</2
        STA /1
        LDA #>/2
        STA 1+/1
endm

;--------------------------------------------------------------------------
;                                                                     MOVE
;--------------------------------------------------------------------------

defm    MOVE

        LDA /1

        endm

;--------------------------------------------------------------------------
;                                                                     DOVE
;--------------------------------------------------------------------------