charset
;same as ASCII
;> d 32 63
	db $00, $00, $00, $00, $00, $00, $00, $00
	db $08, $08, $08, $08, $00, $00, $08, $00
	db $24, $24, $24, $00, $00, $00, $00, $00
	db $24, $24, $7E, $24, $7E, $24, $24, $00
	db $08, $1E, $28, $1C, $0A, $3C, $08, $00
	db $00, $62, $64, $08, $10, $26, $46, $00
	db $30, $48, $48, $30, $4A, $44, $3A, $00
	db $04, $08, $10, $00, $00, $00, $00, $00
	db $04, $08, $10, $10, $10, $08, $04, $00
	db $20, $10, $08, $08, $08, $10, $20, $00
	db $08, $2A, $1C, $3E, $1C, $2A, $08, $00
	db $00, $08, $08, $3E, $08, $08, $00, $00
	db $00, $00, $00, $00, $00, $08, $08, $10
	db $00, $00, $00, $7E, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $18, $18, $00
	db $00, $02, $04, $08, $10, $20, $40, $00
	db $3C, $42, $46, $5A, $62, $42, $3C, $00
	db $08, $18, $28, $08, $08, $08, $3E, $00
	db $3C, $42, $02, $0C, $30, $40, $7E, $00
	db $3C, $42, $02, $1C, $02, $42, $3C, $00
	db $04, $0C, $14, $24, $7E, $04, $04, $00
	db $7E, $40, $78, $04, $02, $44, $38, $00
	db $1C, $20, $40, $7C, $42, $42, $3C, $00
	db $7E, $42, $04, $08, $10, $10, $10, $00
	db $3C, $42, $42, $3C, $42, $42, $3C, $00
	db $3C, $42, $42, $3E, $02, $04, $38, $00
	db $00, $00, $08, $00, $00, $08, $00, $00
	db $00, $00, $08, $00, $00, $08, $08, $10
	db $0E, $18, $30, $60, $30, $18, $0E, $00
	db $00, $00, $7E, $00, $7E, $00, $00, $00
	db $70, $18, $0C, $06, $0C, $18, $70, $00
	db $3C, $42, $02, $0C, $10, $00, $10, $00
;@ 64
;> d 0
	db $1C, $22, $4A, $56, $4C, $20, $1E, $00
;A Z
;> d 1 26
	db $18, $24, $42, $7E, $42, $42, $42, $00
	db $7C, $22, $22, $3C, $22, $22, $7C, $00
	db $1C, $22, $40, $40, $40, $22, $1C, $00
	db $78, $24, $22, $22, $22, $24, $78, $00
	db $7E, $40, $40, $78, $40, $40, $7E, $00
	db $7E, $40, $40, $78, $40, $40, $40, $00
	db $1C, $22, $40, $4E, $42, $22, $1C, $00
	db $42, $42, $42, $7E, $42, $42, $42, $00
	db $1C, $08, $08, $08, $08, $08, $1C, $00
	db $0E, $04, $04, $04, $04, $44, $38, $00
	db $42, $44, $48, $70, $48, $44, $42, $00
	db $40, $40, $40, $40, $40, $40, $7E, $00
	db $42, $66, $5A, $5A, $42, $42, $42, $00
	db $42, $62, $52, $4A, $46, $42, $42, $00
	db $18, $24, $42, $42, $42, $24, $18, $00
	db $7C, $42, $42, $7C, $40, $40, $40, $00
	db $18, $24, $42, $42, $4A, $24, $1A, $00
	db $7C, $42, $42, $7C, $48, $44, $42, $00
	db $3C, $42, $40, $3C, $02, $42, $3C, $00
	db $3E, $08, $08, $08, $08, $08, $08, $00
	db $42, $42, $42, $42, $42, $42, $3C, $00
	db $42, $42, $42, $24, $24, $18, $18, $00
	db $42, $42, $42, $5A, $5A, $66, $42, $00
	db $42, $42, $24, $18, $24, $42, $42, $00
	db $22, $22, $22, $1C, $08, $08, $08, $00
	db $7E, $02, $04, $18, $20, $40, $7E, $00
;[ 91
;> d 27
	db $3C, $20, $20, $20, $20, $20, $3C, $00	
;\ 92 missing
	db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
;] 93
;> d 29
	db $3C, $04, $04, $04, $04, $04, $3C, $00
;^ 94 missing
	db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
;_ 95 missing
	db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
;` 96 missing
	db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
;a z
;> d 257 282
	db $00, $00, $38, $04, $3C, $44, $3A, $00
	db $40, $40, $5C, $62, $42, $62, $5C, $00
	db $00, $00, $3C, $42, $40, $42, $3C, $00
	db $02, $02, $3A, $46, $42, $46, $3A, $00
	db $00, $00, $3C, $42, $7E, $40, $3C, $00
	db $0C, $12, $10, $7C, $10, $10, $10, $00
	db $00, $00, $3A, $46, $46, $3A, $02, $3C
	db $40, $40, $5C, $62, $42, $42, $42, $00
	db $08, $00, $18, $08, $08, $08, $1C, $00
	db $04, $00, $0C, $04, $04, $04, $44, $38
	db $40, $40, $44, $48, $50, $68, $44, $00
	db $18, $08, $08, $08, $08, $08, $1C, $00
	db $00, $00, $76, $49, $49, $49, $49, $00
	db $00, $00, $5C, $62, $42, $42, $42, $00
	db $00, $00, $3C, $42, $42, $42, $3C, $00
	db $00, $00, $5C, $62, $62, $5C, $40, $40
	db $00, $00, $3A, $46, $46, $3A, $02, $02
	db $00, $00, $5C, $62, $40, $40, $40, $00
	db $00, $00, $3E, $40, $3C, $02, $7C, $00
	db $10, $10, $7C, $10, $10, $12, $0C, $00
	db $00, $00, $42, $42, $42, $46, $3A, $00
	db $00, $00, $42, $42, $42, $24, $18, $00
	db $00, $00, $41, $49, $49, $49, $36, $00
	db $00, $00, $42, $24, $18, $24, $42, $00
	db $00, $00, $42, $42, $46, $3A, $02, $3C
	db $00, $00, $7E, $04, $18, $20, $7E, $00
;{ 123 missing
	db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
;| 124 missing
	db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
;} 125 missing
	db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
;~ 126 missing
	db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
; 127
	db $00, $00, $00, $00, $00, $00, $00, $00
end