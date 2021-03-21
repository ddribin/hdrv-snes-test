;============================================================================
; SNES Joypad Input - Polls and stores joypad (controller) information
;============================================================================
;----------------------------------------------------------------------------
; Input Cheat Sheet
; REG_JOY1L = $4218
; $80 = A
; $40 = X
; $20 = L
; $10 = R
; 
; REG_JOY1H = $4219
; $80 = B
; $40 = Y
; $20 = Select
; $10 = Start
; $08 = Up
; $04 = Down
; $02 = Left
; $01 = Right
;----------------------------------------------------------------------------

.BANK 0 SLOT 0
.ORG 0
.SECTION "LoadJoypadCode" SEMIFREE

Joypad:
	LDA REG_HVBJOY
	AND #$01.B			; auto-read joypad status
	BNE Joypad			; read is done when equal to 0

	; low byte first
	LDA Joy1RawL		; load last frame's RAW read of $4218
						; it will be 0 the first time read
	EOR REG_JOY1L		; XOR last frame input with current frame input
						; shows the changes in input
						; buttons just pressed or just released become set.
						; Held or unactive buttons are 0
	AND REG_JOY1L		; AND changes to current frame's input.
						; this ends up leaving you with the only the buttons that are pressed
	STA Joy1PressL		; Store just pressed buttons

	; repeat for high byte
	LDA Joy1RawH		; load last frame's RAW read of $4219
						; it will be 0 the first time read
	EOR REG_JOY1H		; XOR last frame input with current frame input
						; shows the changes in input
						; buttons just pressed or just released become set.
						; Held or unactive buttons are 0
	AND REG_JOY1H		; AND changes to current frame's input.
						; this ends up leaving you with the only the buttons that are pressed
	STA Joy1PressH		; Store just pressed buttons

	LDA REG_JOY1L
	STA Joy1RawL
	LDA REG_JOY1H
	STA Joy1RawH		; Save current frame's entire RAW joypad data for next time	

	; Check if Joypad is standard (not a mouse or superscope, etc) and is connected
	LDA REG_JOYSER0		; Pad 1 - now we read this (after we stored a 0 to it earlier)
	BNE _done			; $4016 returns 0 if not connected, 1 if connected
	STZ Joy1RawL
	STZ Joy1RawH		; if not connected, then clear all recorded input
	STZ Joy1PressL
	STZ Joy1PressH		; it's not valid

_done:
	RTS

.ENDS
