_stdioChar_         = "8000"

        org $8000
cout    rb

        org $9000
start
        ldx #$ff
        txs
pstring ldy #0
l1      lda message,y
        beq l2
        sta	cout
        iny
        bne l1
l2      lda #'\n'
        sta cout
        brk

message db  "This is a test.\0"
				
        org $FFFC
        dw  start