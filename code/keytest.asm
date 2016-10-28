_video_     = "8000,8600,20,20,6,000000,ffffff,880000,aaffee,cc44cc,00cc55,0000aa,eeee77,dd8855,664400,ff7777,333333,777777,aaff66,0088ff,bbbbbb"
_stdioChar_ = "8601"

chio  = $8601
key   = $8600

			org $9000
start	lda key
			beq start
			sta chio
			lda #'\n'
			sta chio
			lda #0
			sta key
			jmp start

			org $FFFC
			dw		start