_stdioChar_      = "8000"
_video_          = "6000,8001,20,20,6,000000,ffffff,880000,aaffee,cc44cc,00cc55,0000aa,eeee77,dd8855,664400,ff7777,333333,777777,aaff66,0088ff,bbbbbb"

chio  = $8000
key		=	$8001

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