; memory mapped i/o 
;
CHIO    EQU $8000   ; character i/o port
INTIO   EQU $8001   ; integer i/o port
HEXIO   EQU $8002   ; hex integer i/o port
RNG     EQU $8003   ; random number generator (read only)

; zero page variables
;
COUNTER RB          ; counter variable

        ORG $9000   ; ROM

START
        LDA #0      ; start counter off with 0
        STA COUNTER
LOOP    INC COUNTER ; bump the counter
        LDA COUNTER
        CMP #6      ; is counter less than 6
        BNE PRINT   ; if so, print
        BRK         ; otherwise, stop
PRINT   STA INTIO    ; send counter value to integer i/o port
        LDA #'\n'   ; now print a line feed
        STA CHIO
        JMP LOOP

        ORG $FFFC   ; reset vector
        DW  START   ; CPU will start executing at 9000