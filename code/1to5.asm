; install required i/o devices
;
_stdioChar_ = "8000" ; add a character i/o "device" and memory map it to address $8000
_stdioInt_  = "8001" ; add an integer i/o "device" and memory map it to address $8001

; memory mapped i/o
;
CHIO    EQU $8000   ; character i/o port
INTIO   EQU $8001   ; integer i/o port

; zero page variables
;
COUNTER RB          ; counter variable

; program ROM
;
        ORG $9000   ; program starts at $9000
START
        LDA #0      ; start counter off with 0
        STA COUNTER
LOOP    INC COUNTER ; bump the counter
        LDA COUNTER
        CMP #6      ; is counter less than 6
        BNE PRINT   ; if so, print
        BRK         ; otherwise, stop
PRINT   STA INTIO   ; send counter value to integer i/o port
        LDA #'\n'   ; now print a line feed
        STA CHIO
        JMP LOOP

        ORG $FFFC   ; reset vector
        DW  START   ; CPU will start executing at 9000