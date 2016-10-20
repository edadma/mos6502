COUNTER RB          ; zero page counter variable

        ORG $8000   ; memory mapped i/o 

COUT    RB          ; character i/o port
IOUT    RB          ; integer i/o port

        ORG $9000   ; ROM

        LDA #0      ; start counter off with 0
        STA COUNTER
LOOP    INC COUNTER ; bump the counter
        LDA COUNTER
        CMP #6      ; is counter less than 6
        BNE PRINT   ; if so, print
        BRK         ; otherwise, stop
PRINT   STA IOUT    ; send counter value to integer i/o port
        LDA #'\n'   ; now print a line feed
        STA COUT
        JMP LOOP

        ORG $FFFC   ; reset vector
        DW  $9000   ; CPU will start executing at 9000