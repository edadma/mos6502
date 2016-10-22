; memory mapped i/o 
;
CHIO    EQU $8000   ; character i/o port
INTIO   EQU $8001   ; integer i/o port
HEXIO   EQU $8002   ; hex integer i/o port
RNG     EQU $8003   ; random number generator (read only)

TEMP    RB          ; temporary variable

        ORG $9000
START
        LDX #$FF
        TXS
        
        LDA INTIO
        JSR MULT10
        STA INTIO
        LDA #'\n'
        STA CHIO
        
        BRK
        
; Multiply A by ten leaving result in A
MULT10  ASL A       ;multiply by 2
        STA TEMP    ;temp store in TEMP
        ASL A       ;again multiply by 2 (*4)
        ASL A       ;again multiply by 2 (*8)
        CLC
        ADC TEMP    ;as result, A = x*8 + x*2
        RTS
        
        ORG $FFFC   ; reset vector
        DW  START   ; CPU will start executing at 9000