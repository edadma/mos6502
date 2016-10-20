				org $8000

cout		rb
iout		rb

				org $9000

				ldx #$ff
				txs
				
pstring	ldy #0
l1			lda message,y
				beq l2
				sta	cout
				iny
				bne l1
l2			lda #'\n'
				sta cout
				brk

message	db	"This is a test.\0"
				
				org $FFFC
				dw	$9000