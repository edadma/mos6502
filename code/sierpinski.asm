; Sierpinski
; Submitted by Anonymous

_video_ = "6000,0,20,20,6,000000,ffffff,880000,aaffee,cc44cc,00cc55,0000aa,eeee77,dd8855,664400,ff7777,333333,777777,aaff66,0088ff,bbbbbb"
_ram_   = "0000-5fff"

video = $6000

  org $9000
  
start:
  lda #$e1
  sta $0
  lda #$5f
  sta $1
  ldy #$20
  lda #$01
write:
  ldx $1
  cpx #$63
  bne write1
  ldx $0
  cpx #$E0
  bne write1
  brk
write1:
  ldx #$00
  eor ($0, x)
  sta ($0),y

  inc $0
  bne write
  inc $1
  bne write
  
  org $fffc
  dw  start