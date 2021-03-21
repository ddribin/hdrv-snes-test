;============================================================================
; SNES Graphics Data - BG Mode 1
;============================================================================
;----------------------------------------------------------------------------
;============================================================================
; Palettes
;----------------------------------------------------------------------------
; 16 colors per palette
; 2 bytes per color => (high) (low) = (0BBBBBGG) (GGGRRRRR)
;----------------------------------------------------------------------------
Palette:
	.incbin "pics\\SNES_HD_retro_logo.clr"
	
	.db $00, $00, $FF, $03, $E0, $7F, $E0, $03	; pal1 = black, yellow, cyan, green,
	.db $1F, $7C, $1F, $00, $00, $7C, $FF, $7F	;		 magenta, red, blue, white,
	.db $00, $00, $00, $00, $00, $00, $00, $00	;		 black, black, black, black,
	.db $00, $00, $00, $00, $00, $00, $00, $00	;		 black, black, black, black

	.db $00, $00, $7B, $6F, $D6, $5A, $52, $4A	; pal2 = black, gray1, gray2, gray3
	.db $AD, $35, $29, $25, $84, $10, $FF, $7F	;		 gray4, gray5, gray6, white,
	.db $00, $00, $00, $00, $00, $00, $00, $00	;		 black, black, black, black,
	.db $00, $00, $00, $00, $00, $00, $00, $00	;		 black, black, black, black

	.db $00, $00, $F7, $02, $E0, $5E, $E0, $02	; pal3 = black, yellow75, cyan75, green75,
	.db $17, $5C, $17, $00, $00, $5C, $F7, $5E	;		 magenta75, red75, blue75, white75,
	.db $00, $00, $00, $00, $00, $00, $00, $00	;		 black, black, black, black,
	.db $00, $00, $00, $00, $00, $00, $00, $00	;		 black, black, black, black
	
;============================================================================
; Tile Definitions
;----------------------------------------------------------------------------
; 8x8 pixel tiles (8 rows of pixels)
; 4 bytes per row of pixels
; two lines of ".db" text below represent one tile
;----------------------------------------------------------------------------
Tiles:

	; Tiles for logo
	.incbin "pics\\SNES_HD_retro_logo.pic"	; 54 tiles long
	
	; Text character tiles; character (decimal) (hex)
	.db $00, $00, $18, $18, $3C, $3C, $66, $66, $66, $66, $7E, $7E, $66, $66, $00, $00 ; A (54) (0x36)
	.db $00, $00, $18, $00, $3C, $00, $66, $00, $66, $00, $7E, $00, $66, $00, $00, $00 

	.db $00, $00, $7C, $7C, $66, $66, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $00, $00 ; B (55) (0x37)
	.db $00, $00, $7C, $00, $66, $00, $7C, $00, $66, $00, $66, $00, $7C, $00, $00, $00 

	.db $00, $00, $3C, $3C, $66, $66, $60, $60, $60, $60, $66, $66, $3C, $3C, $00, $00 ; C (56) (0x38)
	.db $00, $00, $3C, $00, $66, $00, $60, $00, $60, $00, $66, $00, $3C, $00, $00, $00 

	.db $00, $00, $78, $78, $6C, $6C, $66, $66, $66, $66, $6C, $6C, $78, $78, $00, $00 ; D (57) (0x39)
	.db $00, $00, $78, $00, $6C, $00, $66, $00, $66, $00, $6C, $00, $78, $00, $00, $00 

	.db $00, $00, $7E, $7E, $60, $60, $7C, $7C, $60, $60, $60, $60, $7E, $7E, $00, $00 ; E (58) (0x3A)
	.db $00, $00, $7E, $00, $60, $00, $7C, $00, $60, $00, $60, $00, $7E, $00, $00, $00 

	.db $00, $00, $7E, $7E, $60, $60, $7C, $7C, $60, $60, $60, $60, $60, $60, $00, $00 ; F (59) (0x3B)
	.db $00, $00, $7E, $00, $60, $00, $7C, $00, $60, $00, $60, $00, $60, $00, $00, $00 

	.db $00, $00, $3C, $3C, $66, $66, $60, $60, $6E, $6E, $66, $66, $3C, $3C, $00, $00 ; G (60) (0x3C)
	.db $00, $00, $3C, $00, $66, $00, $60, $00, $6E, $00, $66, $00, $3C, $00, $00, $00 

	.db $00, $00, $66, $66, $66, $66, $7E, $7E, $66, $66, $66, $66, $66, $66, $00, $00 ; H (61) (0x3D)
	.db $00, $00, $66, $00, $66, $00, $7E, $00, $66, $00, $66, $00, $66, $00, $00, $00 

	.db $00, $00, $7E, $7E, $18, $18, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00 ; I (62) (0x3E)
	.db $00, $00, $7E, $00, $18, $00, $18, $00, $18, $00, $18, $00, $7E, $00, $00, $00 

	.db $00, $00, $06, $06, $06, $06, $06, $06, $06, $06, $66, $66, $3C, $3C, $00, $00 ; J (63) (0x3F)
	.db $00, $00, $06, $00, $06, $00, $06, $00, $06, $00, $66, $00, $3C, $00, $00, $00 

	.db $00, $00, $66, $66, $6C, $6C, $78, $78, $78, $78, $6C, $6C, $66, $66, $00, $00 ; K (64) (0x40)
	.db $00, $00, $66, $00, $6C, $00, $78, $00, $78, $00, $6C, $00, $66, $00, $00, $00 

	.db $00, $00, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $7E, $7E, $00, $00 ; L (65) (0x41)
	.db $00, $00, $60, $00, $60, $00, $60, $00, $60, $00, $60, $00, $7E, $00, $00, $00 

	.db $00, $00, $63, $63, $77, $77, $7F, $7F, $6B, $6B, $63, $63, $63, $63, $00, $00 ; M (66) (0x42)
	.db $00, $00, $63, $00, $77, $00, $7F, $00, $6B, $00, $63, $00, $63, $00, $00, $00 

	.db $00, $00, $66, $66, $76, $76, $7E, $7E, $7E, $7E, $6E, $6E, $66, $66, $00, $00 ; N (67) (0x43)
	.db $00, $00, $66, $00, $76, $00, $7E, $00, $7E, $00, $6E, $00, $66, $00, $00, $00 

	.db $00, $00, $3C, $3C, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00 ; O (68) (0x44)
	.db $00, $00, $3C, $00, $66, $00, $66, $00, $66, $00, $66, $00, $3C, $00, $00, $00 

	.db $00, $00, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $60, $60, $60, $60, $00, $00 ; P (69) (0x45)
	.db $00, $00, $7C, $00, $66, $00, $66, $00, $7C, $00, $60, $00, $60, $00, $00, $00 

	.db $00, $00, $3C, $3C, $66, $66, $66, $66, $66, $66, $6C, $6C, $36, $36, $00, $00 ; Q (70) (0x46)
	.db $00, $00, $3C, $00, $66, $00, $66, $00, $66, $00, $6C, $00, $36, $00, $00, $00 

	.db $00, $00, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $6C, $6C, $66, $66, $00, $00 ; R (71) (0x47)
	.db $00, $00, $7C, $00, $66, $00, $66, $00, $7C, $00, $6C, $00, $66, $00, $00, $00 

	.db $00, $00, $3C, $3C, $60, $60, $3C, $3C, $06, $06, $06, $06, $3C, $3C, $00, $00 ; S (72) (0x48)
	.db $00, $00, $3C, $00, $60, $00, $3C, $00, $06, $00, $06, $00, $3C, $00, $00, $00 

	.db $00, $00, $7E, $7E, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00 ; T (73) (0x49)
	.db $00, $00, $7E, $00, $18, $00, $18, $00, $18, $00, $18, $00, $18, $00, $00, $00 

	.db $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00 ; U (74) (0x4A)
	.db $00, $00, $66, $00, $66, $00, $66, $00, $66, $00, $66, $00, $7E, $00, $00, $00 

	.db $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $18, $18, $00, $00 ; V (75) (0x4B)
	.db $00, $00, $66, $00, $66, $00, $66, $00, $66, $00, $3C, $00, $18, $00, $00, $00 

	.db $00, $00, $63, $63, $63, $63, $6B, $6B, $7F, $7F, $77, $77, $63, $63, $00, $00 ; W (76) (0x4C)
	.db $00, $00, $63, $00, $63, $00, $6B, $00, $7F, $00, $77, $00, $63, $00, $00, $00 

	.db $00, $00, $66, $66, $66, $66, $3C, $3C, $3C, $3C, $66, $66, $66, $66, $00, $00 ; X (77) (0x4D)
	.db $00, $00, $66, $00, $66, $00, $3C, $00, $3C, $00, $66, $00, $66, $00, $00, $00 

	.db $00, $00, $66, $66, $66, $66, $3C, $3C, $18, $18, $18, $18, $18, $18, $00, $00 ; Y (78) (0x4E)
	.db $00, $00, $66, $00, $66, $00, $3C, $00, $18, $00, $18, $00, $18, $00, $00, $00 

	.db $00, $00, $7E, $7E, $0C, $0C, $18, $18, $30, $30, $60, $60, $7E, $7E, $00, $00 ; Z (79) (0x4F)
	.db $00, $00, $7E, $00, $0C, $00, $18, $00, $30, $00, $60, $00, $7E, $00, $00, $00 

	.db $00, $00, $00, $00, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00 ; a (80) (0x50)
	.db $00, $00, $00, $00, $3C, $00, $06, $00, $3E, $00, $66, $00, $3E, $00, $00, $00 

	.db $00, $00, $60, $60, $60, $60, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $00, $00 ; b (81) (0x51)
	.db $00, $00, $60, $00, $60, $00, $7C, $00, $66, $00, $66, $00, $7C, $00, $00, $00 

	.db $00, $00, $00, $00, $3C, $3C, $60, $60, $60, $60, $60, $60, $3C, $3C, $00, $00 ; c (82) (0x52)
	.db $00, $00, $00, $00, $3C, $00, $60, $00, $60, $00, $60, $00, $3C, $00, $00, $00 

	.db $00, $00, $06, $06, $06, $06, $3E, $3E, $66, $66, $66, $66, $3E, $3E, $00, $00 ; d (83) (0x53)
	.db $00, $00, $06, $00, $06, $00, $3E, $00, $66, $00, $66, $00, $3E, $00, $00, $00 

	.db $00, $00, $00, $00, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00 ; e (84) (0x54)
	.db $00, $00, $00, $00, $3C, $00, $66, $00, $7E, $00, $60, $00, $3C, $00, $00, $00 

	.db $00, $00, $0E, $0E, $18, $18, $3E, $3E, $18, $18, $18, $18, $18, $18, $00, $00 ; f (85) (0x55)
	.db $00, $00, $0E, $00, $18, $00, $3E, $00, $18, $00, $18, $00, $18, $00, $00, $00 

	.db $00, $00, $00, $00, $3E, $3E, $66, $66, $66, $66, $3E, $3E, $06, $06, $7C, $7C ; g (86) (0x56)
	.db $00, $00, $00, $00, $3E, $00, $66, $00, $66, $00, $3E, $00, $06, $00, $7C, $00 

	.db $00, $00, $60, $60, $60, $60, $7C, $7C, $66, $66, $66, $66, $66, $66, $00, $00 ; h (87) (0x57)
	.db $00, $00, $60, $00, $60, $00, $7C, $00, $66, $00, $66, $00, $66, $00, $00, $00 

	.db $00, $00, $0C, $0C, $00, $00, $1C, $1C, $0C, $0C, $0C, $0C, $1E, $1E, $00, $00 ; i (88) (0x58)
	.db $00, $00, $0C, $00, $00, $00, $1C, $00, $0C, $00, $0C, $00, $1E, $00, $00, $00 

	.db $00, $00, $06, $06, $00, $00, $06, $06, $06, $06, $06, $06, $3C, $3C, $00, $00 ; j (89) (0x59)
	.db $00, $00, $06, $00, $00, $00, $06, $00, $06, $00, $06, $00, $3C, $00, $00, $00 

	.db $00, $00, $60, $60, $60, $60, $6C, $6C, $78, $78, $6C, $6C, $66, $66, $00, $00 ; k (90) (0x5A)
	.db $00, $00, $60, $00, $60, $00, $6C, $00, $78, $00, $6C, $00, $66, $00, $00, $00 

	.db $00, $00, $70, $70, $30, $30, $30, $30, $30, $30, $30, $30, $78, $78, $00, $00 ; l (91) (0x5B)
	.db $00, $00, $70, $00, $30, $00, $30, $00, $30, $00, $30, $00, $78, $00, $00, $00 

	.db $00, $00, $00, $00, $66, $66, $7F, $7F, $7F, $7F, $6B, $6B, $63, $63, $00, $00 ; m (92) (0x5C)
	.db $00, $00, $00, $00, $66, $00, $7F, $00, $7F, $00, $6B, $00, $63, $00, $00, $00 

	.db $00, $00, $00, $00, $7C, $7C, $66, $66, $66, $66, $66, $66, $66, $66, $00, $00 ; n (93) (0x5D)
	.db $00, $00, $00, $00, $7C, $00, $66, $00, $66, $00, $66, $00, $66, $00, $00, $00 

	.db $00, $00, $00, $00, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00 ; o (94) (0x5E)
	.db $00, $00, $00, $00, $3C, $00, $66, $00, $66, $00, $66, $00, $3C, $00, $00, $00 

	.db $00, $00, $00, $00, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $60, $60, $60, $60 ; p (95) (0x5F)
	.db $00, $00, $00, $00, $7C, $00, $66, $00, $66, $00, $7C, $00, $60, $00, $60, $00 

	.db $00, $00, $00, $00, $3E, $3E, $66, $66, $66, $66, $3E, $3E, $06, $06, $06, $06 ; q (96) (0x60)
	.db $00, $00, $00, $00, $3E, $00, $66, $00, $66, $00, $3E, $00, $06, $00, $06, $00 

	.db $00, $00, $00, $00, $7C, $7C, $66, $66, $60, $60, $60, $60, $60, $60, $00, $00 ; r (97) (0x61)
	.db $00, $00, $00, $00, $7C, $00, $66, $00, $60, $00, $60, $00, $60, $00, $00, $00 

	.db $00, $00, $00, $00, $3E, $3E, $60, $60, $3C, $3C, $06, $06, $7C, $7C, $00, $00 ; s (98) (0x62)
	.db $00, $00, $00, $00, $3E, $00, $60, $00, $3C, $00, $06, $00, $7C, $00, $00, $00 

	.db $00, $00, $18, $18, $7E, $7E, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00 ; t (99) (0x63)
	.db $00, $00, $18, $00, $7E, $00, $18, $00, $18, $00, $18, $00, $0C, $00, $00, $00 

	.db $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00 ; u (100) (0x64)
	.db $00, $00, $00, $00, $66, $00, $66, $00, $66, $00, $66, $00, $3C, $00, $00, $00 

	.db $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $3C, $3C, $18, $18, $00, $00 ; v (101) (0x65)
	.db $00, $00, $00, $00, $66, $00, $66, $00, $66, $00, $3C, $00, $18, $00, $00, $00 

	.db $00, $00, $00, $00, $63, $63, $6B, $6B, $7F, $7F, $3E, $3E, $36, $36, $00, $00 ; w (102) (0x66)
	.db $00, $00, $00, $00, $63, $00, $6B, $00, $7F, $00, $3E, $00, $36, $00, $00, $00 

	.db $00, $00, $00, $00, $66, $66, $3C, $3C, $18, $18, $3C, $3C, $66, $66, $00, $00 ; x (103) (0x67)
	.db $00, $00, $00, $00, $66, $00, $3C, $00, $18, $00, $3C, $00, $66, $00, $00, $00 

	.db $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $0C, $0C, $78, $78 ; y (104) (0x68)
	.db $00, $00, $00, $00, $66, $00, $66, $00, $66, $00, $3E, $00, $0C, $00, $78, $00 

	.db $00, $00, $00, $00, $7E, $7E, $0C, $0C, $18, $18, $30, $30, $7E, $7E, $00, $00 ; z (105) (0x69)
	.db $00, $00, $00, $00, $7E, $00, $0C, $00, $18, $00, $30, $00, $7E, $00, $00, $00 

	.db $00, $00, $3C, $3C, $66, $66, $6E, $6E, $76, $76, $66, $66, $3C, $3C, $00, $00 ; 0 (106) (0x6A)
	.db $00, $00, $3C, $00, $66, $00, $6E, $00, $76, $00, $66, $00, $3C, $00, $00, $00 

	.db $00, $00, $18, $18, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00 ; 1 (107) (0x6B)
	.db $00, $00, $18, $00, $38, $00, $18, $00, $18, $00, $18, $00, $7E, $00, $00, $00 

	.db $00, $00, $3C, $3C, $66, $66, $0C, $0C, $18, $18, $30, $30, $7E, $7E, $00, $00 ; 2 (108) (0x6C)
	.db $00, $00, $3C, $00, $66, $00, $0C, $00, $18, $00, $30, $00, $7E, $00, $00, $00 

	.db $00, $00, $7E, $7E, $0C, $0C, $18, $18, $0C, $0C, $66, $66, $3C, $3C, $00, $00 ; 3 (109) (0x6D)
	.db $00, $00, $7E, $00, $0C, $00, $18, $00, $0C, $00, $66, $00, $3C, $00, $00, $00 

	.db $00, $00, $0C, $0C, $1C, $1C, $3C, $3C, $6C, $6C, $7E, $7E, $0C, $0C, $00, $00 ; 4 (110) (0x6E)
	.db $00, $00, $0C, $00, $1C, $00, $3C, $00, $6C, $00, $7E, $00, $0C, $00, $00, $00 

	.db $00, $00, $7E, $7E, $60, $60, $7C, $7C, $06, $06, $66, $66, $3C, $3C, $00, $00 ; 5 (111) (0x6F)
	.db $00, $00, $7E, $00, $60, $00, $7C, $00, $06, $00, $66, $00, $3C, $00, $00, $00 

	.db $00, $00, $3C, $3C, $60, $60, $7C, $7C, $66, $66, $66, $66, $3C, $3C, $00, $00 ; 6 (112) (0x70)
	.db $00, $00, $3C, $00, $60, $00, $7C, $00, $66, $00, $66, $00, $3C, $00, $00, $00 

	.db $00, $00, $7E, $7E, $06, $06, $0C, $0C, $18, $18, $30, $30, $30, $30, $00, $00 ; 7 (113) (0x71)
	.db $00, $00, $7E, $00, $06, $00, $0C, $00, $18, $00, $30, $00, $30, $00, $00, $00 

	.db $00, $00, $3C, $3C, $66, $66, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00 ; 8 (114) (0x72)
	.db $00, $00, $3C, $00, $66, $00, $3C, $00, $66, $00, $66, $00, $3C, $00, $00, $00 

	.db $00, $00, $3C, $3C, $66, $66, $3E, $3E, $06, $06, $0C, $0C, $38, $38, $00, $00 ; 9 (115) (0x73)
	.db $00, $00, $3C, $00, $66, $00, $3E, $00, $06, $00, $0C, $00, $38, $00, $00, $00 

	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0C, $0C, $0C, $0C, $18, $18 ; comma (116) (0x74)
	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0C, $00, $0C, $00, $18, $00 

	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0C, $0C, $0C, $0C, $00, $00 ; period (117) (0x75)
	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0C, $00, $0C, $00, $00, $00 

	.db $00, $00, $00, $00, $00, $00, $7E, $7E, $00, $00, $00, $00, $00, $00, $00, $00 ; dash (118) (0x76)
	.db $00, $00, $00, $00, $00, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00, $00 

	.db $00, $00, $00, $00, $18, $18, $18, $18, $00, $00, $18, $18, $18, $18, $00, $00 ; colon (119) (0x77)
	.db $00, $00, $00, $00, $18, $00, $18, $00, $00, $00, $18, $00, $18, $00, $00, $00 

	.db $00, $00, $00, $00, $00, $00, $7E, $7E, $00, $00, $7E, $7E, $00, $00, $00, $00 ; equals (120) (0x78)
	.db $00, $00, $00, $00, $00, $00, $7E, $00, $00, $00, $7E, $00, $00, $00, $00, $00 

	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; space (121) (0x79)
	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	
	.db $00, $00, $06, $06, $0C, $0C, $18, $18, $30, $30, $60, $60, $40, $40, $00, $00 ; slash (122) (0x7A)
	.db $00, $00, $06, $00, $0C, $00, $18, $00, $30, $00, $60, $00, $40, $00, $00, $00 

	
	; Tiles for test pattens
	.db $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA ; singlecolumn(123)
	.db $AA, $00, $AA, $00, $AA, $00, $AA, $00, $AA, $00, $AA, $00, $AA, $00, $AA, $00 ; fast bandwidth - alternate color every pixel

	.db $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC ; doublecolumn(124)
	.db $CC, $00, $CC, $00, $CC, $00, $CC, $00, $CC, $00, $CC, $00, $CC, $00, $CC, $00 ; slow bandwidth - alternate color every 2nd pixel

    .db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00	; solid color tile using 1st color in palette(125)
	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	
	.db $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00	; solid color tile using 2nd color in palette(126)
	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	
    .db $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF	; solid color tile using 3rd color in palette(127)
	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	
	.db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF	; solid color tile using 4th color in palette(128)
	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	
	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00	; solid color tile using 5th color in palette(129)
	.db $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00
	
	.db $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00	; solid color tile using 6th color in palette(130)
	.db $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00
	
    .db $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF	; solid color tile using 7th color in palette(131)
	.db $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00
	
	.db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF	; solid color tile using 8th color in palette(132)
	.db $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00, $FF, $00

;============================================================================
; Tilemaps for test patterns (quad row)
; NOTE: Changed from single row to quad row because of shorter vblank time in 239p video mode.
;       Many short DMA transfers to VRAM are much less efficient than fewer longer transfers.
;----------------------------------------------------------------------------
; (32 tiles per row) x (4 rows) = 128 tiles
; 2 bytes per tile => (high) (low) = (vhopppcc) (cccccccc)
; v/h        = Vertical/Horizontal flip the tile.
; o          = Tile priority.
; ppp        = Tile palette. The number of entries in the palette depends on the Mode and the BG.
; cccccccccc = Tile number.
;----------------------------------------------------------------------------
Color:
	; row 1
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $7E, $04, $7E, $04, $7E, $04, $7E, $04	; 4 tiles using tile 126 and pal1 (yellow)
	.db $7F, $04, $7F, $04, $7F, $04, $7F, $04	; 4 tiles using tile 127 and pal1 (cyan)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $82, $04, $82, $04, $82, $04, $82, $04	; 4 tiles using tile 130 and pal1 (red)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $7D, $04, $7D, $04, $7D, $04, $7D, $04	; 4 tiles using tile 125 and pal1 (black)
	; row 2
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $7E, $04, $7E, $04, $7E, $04, $7E, $04	; 4 tiles using tile 126 and pal1 (yellow)
	.db $7F, $04, $7F, $04, $7F, $04, $7F, $04	; 4 tiles using tile 127 and pal1 (cyan)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $82, $04, $82, $04, $82, $04, $82, $04	; 4 tiles using tile 130 and pal1 (red)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $7D, $04, $7D, $04, $7D, $04, $7D, $04	; 4 tiles using tile 125 and pal1 (black)
	; row 3
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $7E, $04, $7E, $04, $7E, $04, $7E, $04	; 4 tiles using tile 126 and pal1 (yellow)
	.db $7F, $04, $7F, $04, $7F, $04, $7F, $04	; 4 tiles using tile 127 and pal1 (cyan)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $82, $04, $82, $04, $82, $04, $82, $04	; 4 tiles using tile 130 and pal1 (red)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $7D, $04, $7D, $04, $7D, $04, $7D, $04	; 4 tiles using tile 125 and pal1 (black)
	; row 4
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $7E, $04, $7E, $04, $7E, $04, $7E, $04	; 4 tiles using tile 126 and pal1 (yellow)
	.db $7F, $04, $7F, $04, $7F, $04, $7F, $04	; 4 tiles using tile 127 and pal1 (cyan)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $82, $04, $82, $04, $82, $04, $82, $04	; 4 tiles using tile 130 and pal1 (red)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $7D, $04, $7D, $04, $7D, $04, $7D, $04	; 4 tiles using tile 125 and pal1 (black)
	
Color75:
	; row 1
	.db $84, $0C, $84, $0C, $84, $0C, $84, $0C	; 4 tiles using tile 132 and pal3 (white75)
	.db $7E, $0C, $7E, $0C, $7E, $0C, $7E, $0C	; 4 tiles using tile 126 and pal3 (yellow75)
	.db $7F, $0C, $7F, $0C, $7F, $0C, $7F, $0C	; 4 tiles using tile 127 and pal3 (cyan75)
	.db $80, $0C, $80, $0C, $80, $0C, $80, $0C	; 4 tiles using tile 128 and pal3 (green75)
	.db $81, $0C, $81, $0C, $81, $0C, $81, $0C	; 4 tiles using tile 129 and pal3 (magenta75)
	.db $82, $0C, $82, $0C, $82, $0C, $82, $0C	; 4 tiles using tile 130 and pal3 (red75)
	.db $83, $0C, $83, $0C, $83, $0C, $83, $0C	; 4 tiles using tile 131 and pal3 (blue75)
	.db $7D, $0C, $7D, $0C, $7D, $0C, $7D, $0C	; 4 tiles using tile 125 and pal3 (black)
	; row 2
	.db $84, $0C, $84, $0C, $84, $0C, $84, $0C	; 4 tiles using tile 132 and pal3 (white75)
	.db $7E, $0C, $7E, $0C, $7E, $0C, $7E, $0C	; 4 tiles using tile 126 and pal3 (yellow75)
	.db $7F, $0C, $7F, $0C, $7F, $0C, $7F, $0C	; 4 tiles using tile 127 and pal3 (cyan75)
	.db $80, $0C, $80, $0C, $80, $0C, $80, $0C	; 4 tiles using tile 128 and pal3 (green75)
	.db $81, $0C, $81, $0C, $81, $0C, $81, $0C	; 4 tiles using tile 129 and pal3 (magenta75)
	.db $82, $0C, $82, $0C, $82, $0C, $82, $0C	; 4 tiles using tile 130 and pal3 (red75)
	.db $83, $0C, $83, $0C, $83, $0C, $83, $0C	; 4 tiles using tile 131 and pal3 (blue75)
	.db $7D, $0C, $7D, $0C, $7D, $0C, $7D, $0C	; 4 tiles using tile 125 and pal3 (black)
	; row 3
	.db $84, $0C, $84, $0C, $84, $0C, $84, $0C	; 4 tiles using tile 132 and pal3 (white75)
	.db $7E, $0C, $7E, $0C, $7E, $0C, $7E, $0C	; 4 tiles using tile 126 and pal3 (yellow75)
	.db $7F, $0C, $7F, $0C, $7F, $0C, $7F, $0C	; 4 tiles using tile 127 and pal3 (cyan75)
	.db $80, $0C, $80, $0C, $80, $0C, $80, $0C	; 4 tiles using tile 128 and pal3 (green75)
	.db $81, $0C, $81, $0C, $81, $0C, $81, $0C	; 4 tiles using tile 129 and pal3 (magenta75)
	.db $82, $0C, $82, $0C, $82, $0C, $82, $0C	; 4 tiles using tile 130 and pal3 (red75)
	.db $83, $0C, $83, $0C, $83, $0C, $83, $0C	; 4 tiles using tile 131 and pal3 (blue75)
	.db $7D, $0C, $7D, $0C, $7D, $0C, $7D, $0C	; 4 tiles using tile 125 and pal3 (black)
	; row 4
	.db $84, $0C, $84, $0C, $84, $0C, $84, $0C	; 4 tiles using tile 132 and pal3 (white75)
	.db $7E, $0C, $7E, $0C, $7E, $0C, $7E, $0C	; 4 tiles using tile 126 and pal3 (yellow75)
	.db $7F, $0C, $7F, $0C, $7F, $0C, $7F, $0C	; 4 tiles using tile 127 and pal3 (cyan75)
	.db $80, $0C, $80, $0C, $80, $0C, $80, $0C	; 4 tiles using tile 128 and pal3 (green75)
	.db $81, $0C, $81, $0C, $81, $0C, $81, $0C	; 4 tiles using tile 129 and pal3 (magenta75)
	.db $82, $0C, $82, $0C, $82, $0C, $82, $0C	; 4 tiles using tile 130 and pal3 (red75)
	.db $83, $0C, $83, $0C, $83, $0C, $83, $0C	; 4 tiles using tile 131 and pal3 (blue75)
	.db $7D, $0C, $7D, $0C, $7D, $0C, $7D, $0C	; 4 tiles using tile 125 and pal3 (black)
	
Gray:
	; row 1
	.db $84, $08, $84, $08, $84, $08, $84, $08	; 4 tiles using tile 132 and pal2 (white)
	.db $7E, $08, $7E, $08, $7E, $08, $7E, $08	; 4 tiles using tile 126 and pal2 (gray1)
	.db $7F, $08, $7F, $08, $7F, $08, $7F, $08	; 4 tiles using tile 127 and pal2 (gray2)
	.db $80, $08, $80, $08, $80, $08, $80, $08	; 4 tiles using tile 128 and pal2 (gray3)
	.db $81, $08, $81, $08, $81, $08, $81, $08	; 4 tiles using tile 129 and pal2 (gray4)
	.db $82, $08, $82, $08, $82, $08, $82, $08	; 4 tiles using tile 130 and pal2 (gray5)
	.db $83, $08, $83, $08, $83, $08, $83, $08	; 4 tiles using tile 131 and pal2 (gray6)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	; row 2
	.db $84, $08, $84, $08, $84, $08, $84, $08	; 4 tiles using tile 132 and pal2 (white)
	.db $7E, $08, $7E, $08, $7E, $08, $7E, $08	; 4 tiles using tile 126 and pal2 (gray1)
	.db $7F, $08, $7F, $08, $7F, $08, $7F, $08	; 4 tiles using tile 127 and pal2 (gray2)
	.db $80, $08, $80, $08, $80, $08, $80, $08	; 4 tiles using tile 128 and pal2 (gray3)
	.db $81, $08, $81, $08, $81, $08, $81, $08	; 4 tiles using tile 129 and pal2 (gray4)
	.db $82, $08, $82, $08, $82, $08, $82, $08	; 4 tiles using tile 130 and pal2 (gray5)
	.db $83, $08, $83, $08, $83, $08, $83, $08	; 4 tiles using tile 131 and pal2 (gray6)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	; row 3
	.db $84, $08, $84, $08, $84, $08, $84, $08	; 4 tiles using tile 132 and pal2 (white)
	.db $7E, $08, $7E, $08, $7E, $08, $7E, $08	; 4 tiles using tile 126 and pal2 (gray1)
	.db $7F, $08, $7F, $08, $7F, $08, $7F, $08	; 4 tiles using tile 127 and pal2 (gray2)
	.db $80, $08, $80, $08, $80, $08, $80, $08	; 4 tiles using tile 128 and pal2 (gray3)
	.db $81, $08, $81, $08, $81, $08, $81, $08	; 4 tiles using tile 129 and pal2 (gray4)
	.db $82, $08, $82, $08, $82, $08, $82, $08	; 4 tiles using tile 130 and pal2 (gray5)
	.db $83, $08, $83, $08, $83, $08, $83, $08	; 4 tiles using tile 131 and pal2 (gray6)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	; row 4
	.db $84, $08, $84, $08, $84, $08, $84, $08	; 4 tiles using tile 132 and pal2 (white)
	.db $7E, $08, $7E, $08, $7E, $08, $7E, $08	; 4 tiles using tile 126 and pal2 (gray1)
	.db $7F, $08, $7F, $08, $7F, $08, $7F, $08	; 4 tiles using tile 127 and pal2 (gray2)
	.db $80, $08, $80, $08, $80, $08, $80, $08	; 4 tiles using tile 128 and pal2 (gray3)
	.db $81, $08, $81, $08, $81, $08, $81, $08	; 4 tiles using tile 129 and pal2 (gray4)
	.db $82, $08, $82, $08, $82, $08, $82, $08	; 4 tiles using tile 130 and pal2 (gray5)
	.db $83, $08, $83, $08, $83, $08, $83, $08	; 4 tiles using tile 131 and pal2 (gray6)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)

White:
	; row 1
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	; row 2
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	; row 3
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	; row 4
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)
	.db $84, $04, $84, $04, $84, $04, $84, $04	; 4 tiles using tile 132 and pal1 (white)

Green:
	; row 1
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	; row 2
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	; row 3
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	; row 4
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	.db $80, $04, $80, $04, $80, $04, $80, $04	; 4 tiles using tile 128 and pal1 (green)
	
Magenta:
	; row 1
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	; row 2
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	; row 3
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	; row 4
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)
	.db $81, $04, $81, $04, $81, $04, $81, $04	; 4 tiles using tile 129 and pal1 (magenta)

Blue:
	; row 1
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	; row 2
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	; row 3
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	; row 4
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)
	.db $83, $04, $83, $04, $83, $04, $83, $04	; 4 tiles using tile 131 and pal1 (blue)

Black:
	; row 1
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	; row 2
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	; row 3
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	; row 4
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	.db $7D, $08, $7D, $08, $7D, $08, $7D, $08	; 4 tiles using tile 125 and pal2 (black)
	

SlowBand:
	; row 1
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	; row 2
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	; row 3
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	; row 4
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	.db $7C, $04, $7C, $04, $7C, $04, $7C, $04	; 4 tiles using doublecolumn pattern and pal1 (black&white)
	
FastBand:
	; row 1
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	; row 2
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	; row 3
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	; row 4
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	.db $7B, $04, $7B, $04, $7B, $04, $7B, $04	; 4 tiles using singlecolumn pattern and pal1 (black&white)
	
;============================================================================
; Flashing object Tilemap
;----------------------------------------------------------------------------
FlashBox:
	.db $7F, $04, $7F, $04, $7F, $04, $7F, $04	; 4 tiles using tile 127 and pal1 (cyan)
	.db $7F, $04, $7F, $04, $7F, $04, $7F, $04	; 4 tiles using tile 127 and pal1 (cyan)
	
;============================================================================
; Pre-defined Tilemaps
;----------------------------------------------------------------------------
Splash:
	.incbin "text\\splashscreen.map"

Help:
	.incbin "text\\helpscreen.map"
	
LeftAud:
	.incbin "text\\leftaud.map"

RightAud:
	.incbin "text\\rightaud.map"
	
Scanmode_interlace:
	.incbin "text\\scanmode_interlace.map"
	
Scanmode_noninterlace:
	.incbin "text\\scanmode_noninterlace.map"

Interlace:
	.incbin "text\\interlace.map"
	
Noninterlace:
	.incbin "text\\noninterlace.map"

Logo:
	.incbin "pics\\SNES_HD_retro_logo.map"

r256x224:
	.incbin "text\\r256x224.map"
r256x239:
	.incbin "text\\r256x239.map"
r256x448:
	.incbin "text\\r256x448.map"
r256x478:
	.incbin "text\\r256x478.map"
r512x224:
	.incbin "text\\r512x224.map"
r512x239:
	.incbin "text\\r512x239.map"
r512x448:
	.incbin "text\\r512x448.map"
r512x478:
	.incbin "text\\r512x478.map"

Timer:
	.db $6F, $04	;5
	.db $6E, $04	;4
	.db $6D, $04	;3
	.db $6C, $04	;2
	.db $6B, $04	;1
	.db $6A, $04	;0