_stdioChar_ = "8000"

        org $8000
chio    rb

        org $9000
start   ldy #0
.1      lda message,y
        beq .2
        sta	chio
        iny
        bne .1
.2      lda #'\n'
        sta chio
        brk

message db  "Hello World!\0"
				
        org $FFFC
        dw  start