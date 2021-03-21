;============================================================================
; SNES SPC700 Sound Data - Creates a "ding" sound effect on each channel
;============================================================================
;----------------------------------------------------------------------------
.BANK 0 SLOT 0
.ORG 0
.SECTION "PlaySoundCode" SEMIFREE

; Volume Level Variables
.EQU VOL_ON		$7F
.EQU VOL_OFF	$00

PlaySoundL:
	JSR spc_wait_boot
	; Upload code to SPC at $0200
	LDY #$0200.W
	JSR spc_begin_upload	; also sets Y to 0
SoundLoopL:
	LDA spc_code,y
	JSR spc_upload_byte		; also increments Y
	CPY #(spc_code_end-spc_code).W
	BNE SoundLoopL
	
	; Start executing SPC code at $0204
	LDY #$0204.W
	JSR spc_execute
	RTS

PlaySoundR:
	JSR spc_wait_boot
	; Upload code to SPC at $0200
	LDY #$0200.W
	JSR spc_begin_upload	; also sets Y to 0
SoundLoopR:
	LDA spc_code2,y
	JSR spc_upload_byte		; also increments Y
	CPY #(spc_code_end2-spc_code2).W
	BNE SoundLoopR
	
	; Start executing SPC code at $0204
	LDY #$0204.W
	JSR spc_execute
	RTS

; Code to upload to SPC
spc_code:
;.org $200
;
;directory:
.db  $07,$02
.db  $07,$02

.db  $5F,$19,$02 ; jmp   !$0219

;sample:
;sample_loop:
.db $b0,$78,$78,$78,$78,$78,$78,$78,$78
.db $b3,$78,$78,$78,$78,$78,$78,$78,$78

.db  $00         ; nop
.db  $00         ; nop
.db  $8F,$6C,$F2 ; mov   $f2, #$6c
; wdsp flg,$20
.db  $8F,$20,$F3 ; mov   $f3, #$20
.db  $8F,$4C,$F2 ; mov   $f2, #$4c
; wdsp kon,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$5C,$F2 ; mov   $f2, #$5c
; wdsp koff,$FF
.db  $8F,$FF,$F3 ; mov   $f3, #$ff 
.db  $8F,$5D,$F2 ; mov   $f2, #$5d 
; wdsp dir,>directory ???
.db  $8F,$02,$F3 ; mov   $f3, #$02
.db  $8F,$00,$F2 ; mov   $f2, #$00
; wdsp vvoll,$7F
.db  $8F,VOL_ON,$F3 ; mov   $f3, #$7f //CHANGE THIS TO CHANGE LEFT VOLUME
.db  $8F,$01,$F2 ; mov   $f2, #$01 
; wdsp vvolr,$7F
.db  $8F,VOL_OFF,$F3 ; mov   $f3, #$7f //CHANGE THIS TO CHANAGE RIGHT VOLUME
.db  $8F,$02,$F2 ; mov   $f2, #$02
; wdsp vpitchl,$00
.db  $8F,$04,$F3 ; mov   $f3, #$00 
.db  $8F,$03,$F2 ; mov   $f2, #$03 
; wdsp vpitchh,$02
.db  $8F,$01,$F3 ; mov   $f3, #$02 
.db  $8F,$04,$F2 ; mov   $f2, #$04 
; wdsp vsrcn,0
.db  $8F,$00,$F3 ; mov   $f3, #$00 
.db  $8F,$05,$F2 ; mov   $f2, #$05 
; wdsp vadsr0,$C3
.db  $8F,$8E,$F3 ; mov   $f3, #$c3 
.db  $8F,$06,$F2 ; mov   $f2, #$06 
; wdsp vadsr1,$2F
.db  $8F,$F3,$F3 ; mov   $f3, #$2f 
.db  $8F,$07,$F2 ; mov   $f2, #$07
; wdsp vgain,$CF
.db  $8F,$CF,$F3 ; mov   $f3, #$cf
.db  $8F,$5C,$F2 ; mov   $f2, #$5c
; wdsp koff,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$3D,$F2 ; mov   $f2, #$3d
; wdsp non,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$4D,$F2 ; mov   $f2, #$4d
; wdsp eon,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$0C,$F2 ; mov   $f2, #$0c
; wdsp mvoll,$7F
.db  $8F,$7F,$F3 ; mov   $f3, #$7f
.db  $8F,$1C,$F2 ; mov   $f2, #$1c
; wdsp mvolr,$7F
.db  $8F,$7F,$F3 ; mov   $f3, #$7f
.db  $8F,$2C,$F2 ; mov   $f2, #$2c
; wdsp evoll,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$3C,$F2 ; mov   $f2, #$3c
; wdsp evolr,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$4C,$F2 ; mov   $f2, #$4c
; wdsp kon,$01
.db  $8F,$01,$F3 ; mov   $f3, #$01
.db  $5F,$C0,$FF ; jmp   !$ffc0
; jmp !$FFC0
spc_code_end:

spc_code2:
;.org $200
;
;directory:
.db  $07,$02
.db  $07,$02

.db  $5F,$19,$02 ; jmp   !$0219

;sample:
;sample_loop:
.db $b0,$78,$78,$78,$78,$78,$78,$78,$78
.db $b3,$78,$78,$78,$78,$78,$78,$78,$78

.db  $00         ; nop
.db  $00         ; nop
.db  $8F,$6C,$F2 ; mov   $f2, #$6c
; wdsp flg,$20
.db  $8F,$20,$F3 ; mov   $f3, #$20
.db  $8F,$4C,$F2 ; mov   $f2, #$4c
; wdsp kon,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$5C,$F2 ; mov   $f2, #$5c
; wdsp koff,$FF
.db  $8F,$FF,$F3 ; mov   $f3, #$ff 
.db  $8F,$5D,$F2 ; mov   $f2, #$5d 
; wdsp dir,>directory ???
.db  $8F,$02,$F3 ; mov   $f3, #$02
.db  $8F,$00,$F2 ; mov   $f2, #$00
; wdsp vvoll,$7F
.db  $8F,VOL_OFF,$F3 ; mov   $f3, #$7f //CHANGE THIS TO CHANGE LEFT VOLUME
.db  $8F,$01,$F2 ; mov   $f2, #$01 
; wdsp vvolr,$7F
.db  $8F,VOL_ON,$F3 ; mov   $f3, #$7f //CHANGE THIS TO CHANAGE RIGHT VOLUME
.db  $8F,$02,$F2 ; mov   $f2, #$02
; wdsp vpitchl,$00
.db  $8F,$04,$F3 ; mov   $f3, #$00 
.db  $8F,$03,$F2 ; mov   $f2, #$03 
; wdsp vpitchh,$02
.db  $8F,$01,$F3 ; mov   $f3, #$02 
.db  $8F,$04,$F2 ; mov   $f2, #$04 
; wdsp vsrcn,0
.db  $8F,$00,$F3 ; mov   $f3, #$00 
.db  $8F,$05,$F2 ; mov   $f2, #$05 
; wdsp vadsr0,$C3
.db  $8F,$8E,$F3 ; mov   $f3, #$c3 
.db  $8F,$06,$F2 ; mov   $f2, #$06 
; wdsp vadsr1,$2F
.db  $8F,$F3,$F3 ; mov   $f3, #$2f 
.db  $8F,$07,$F2 ; mov   $f2, #$07
; wdsp vgain,$CF
.db  $8F,$CF,$F3 ; mov   $f3, #$cf
.db  $8F,$5C,$F2 ; mov   $f2, #$5c
; wdsp koff,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$3D,$F2 ; mov   $f2, #$3d
; wdsp non,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$4D,$F2 ; mov   $f2, #$4d
; wdsp eon,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$0C,$F2 ; mov   $f2, #$0c
; wdsp mvoll,$7F
.db  $8F,$7F,$F3 ; mov   $f3, #$7f
.db  $8F,$1C,$F2 ; mov   $f2, #$1c
; wdsp mvolr,$7F
.db  $8F,$7F,$F3 ; mov   $f3, #$7f
.db  $8F,$2C,$F2 ; mov   $f2, #$2c
; wdsp evoll,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$3C,$F2 ; mov   $f2, #$3c
; wdsp evolr,0
.db  $8F,$00,$F3 ; mov   $f3, #$00
.db  $8F,$4C,$F2 ; mov   $f2, #$4c
; wdsp kon,$01
.db  $8F,$01,$F3 ; mov   $f3, #$01
.db  $5F,$C0,$FF ; jmp   !$ffc0
; jmp !$FFC0
spc_code_end2:

; SPC code that generated the above
; (wdsp writes value to dsp register)
;
; wdsp flg,$20
; wdsp kon,0
; wdsp koff,$FF
; wdsp dir,>directory
;
; wdsp vvoll,$7F
; wdsp vvolr,$7F
; wdsp vpitchl,$00 
; wdsp vpitchh,$02 
; wdsp vsrcn,0
; wdsp vadsr0,$C3
; wdsp vadsr1,$2F
; wdsp vgain,$CF
;
; wdsp koff,0
; wdsp non,0
; wdsp eon,0
; wdsp mvoll,$7F
; wdsp mvolr,$7F
; wdsp evoll,0
; wdsp evolr,0
;
; wdsp kon,$01
;
; jmp !$FFC0



; High-level interface to SPC-700 bootloader
;
; 1. Call spc_wait_boot
; 2. To upload data:
;       A. Call spc_begin_upload
;       B. Call spc_upload_byte any number of times
;       C. Go back to A to upload to different addr
; 3. To begin execution, call spc_execute
;
; Have your SPC code jump to $FFC0 to re-run bootloader.
; Be sure to call spc_wait_boot after that.


; Waits for SPC to finish booting. Call before first
; using SPC or after bootrom has been re-run.
; Preserved: X, Y
spc_wait_boot:
	LDA #$AA.B
-	CMP REG_APUIO0
	BNE -

	; Clear in case it already has $CC in it
	; (this actually occurred in testing)
	STA REG_APUIO0

	LDA #$BB.B
-	CMP REG_APUIO1
	BNE -
	RTS

; Starts upload to SPC addr Y and sets Y to
; 0 for use as index with spc_upload_byte.
; Preserved: X
spc_begin_upload:
	STY REG_APUIO2

	; Send command
	LDA REG_APUIO0
	CLC
	ADC #$22.B
	BNE +  ; special case fully verified
	INC A
+	STA REG_APUIO1
	STA REG_APUIO0

	; Wait for acknowledgement
-	CMP REG_APUIO0
	BNE -

	; Initialize index
	LDY #$0000.W
	RTS

; Uploads byte A to SPC and increments Y. The low byte
; of Y must not be disturbed between bytes.
; Preserved: X
spc_upload_byte:
	STA REG_APUIO1

	; Signal that it's ready
	TYA
	STA REG_APUIO0
	INY

	; Wait for acknowledgement
-	CMP REG_APUIO0
	BNE -
	RTS

; Starts executing at SPC addr Y
; Preserved: X, Y
spc_execute:
	STY REG_APUIO2

	STZ REG_APUIO1

	LDA REG_APUIO0
	CLC
	ADC #$22.B
	STA REG_APUIO0
	
	; Wait for acknowledgement
-	CMP REG_APUIO0
	BNE -
	RTS
.ENDS
