; Super Nintendo HDRV Test Software
; Version: v1.4
; Date: 2018-01-27
; CHANGE NOTES from v1.0 -> v1.1: Added Bandwidth Test Patterns
; CHANGE NOTES from v1.1 -> v1.2: Added 75% (really 74.19%) Colorbar Test Pattern
; CHANGE NOTES from v1.2 -> v1.3: Added full green screen test pattern
;								  Replaced all instances of "Factory" with "HDRV"
;								  Increased ROM size from 2Mbit to 4Mbit for better compatibility with flash carts
;								  Changed output ROM file extension from .SMC to .SFC
; CHANGE NOTES from v1.3 -> v1.4: Added ability to change to 224x239 screen size (239p mode)
;								  Changed DrawLines routine to draw 4 lines at a time to support reduced vblank time during 239p mode
;								  Added alternating frame 60Hz/50Hz flash test for checking 240p processing
; 
; First written in 2015 by HD Retrovision LLC
; http://www.hdretrovision.com
; Authors: Ste Kulov, Nick Mueller, Chris Sowa
; 
; To the extent possible under law, the author(s) have dedicated all copyright
; and related and neighboring rights to this software to the public domain worldwide.
; This software is distributed without any warranty.
; 
; You should have received a copy of the CC0 Public Domain Dedication along with this software.
; If not, see <http://creativecommons.org/publicdomain/zero/1.0/>. 

;============================================================================
; Includes
;============================================================================
;== Include SNES Definitions ==
.INCLUDE "snes.inc"

;== Include MemoryMap, Vector Table, and HeaderInfo ==
.INCLUDE "header.inc"

;== Include SNES Initialization Routines ==
.INCLUDE "initSNES.inc"

;== Include SNES Joypad Input Routines ==
.INCLUDE "joypad.inc"

;== Include SPC700 sound test code ==
.INCLUDE "sound.inc"

;============================================================================
; Definitions
;============================================================================
; VRAM Tilemap Positions
.EQU TILEMAP_TOP $2000		; the first address of the tilemap in VRAM
.EQU LOGO_START $2065		; the tilemap address where the logo is properly positioned
.EQU TIMER_POS $2274		; countdown timer in the splash screen
.EQU RESPOS_SPLASH $2173	; screen size text within the splash screen
.EQU RESPOS_HELP $2213		; screen size text within the help screen
.EQU RESPOS_MAIN $22E3		; screen size text within main test patterns
.EQU SCANPOS_SPLASH $218F	; scan mode text within the splash screen
.EQU SCANPOS_HELP $2230		; scan mode text within the help screen
.EQU SCANPOS_MAIN $22E3		; scan mode text within main test patterns
.EQU LAUDIO_POS $22E1		; left audio text indicator
.EQU RAUDIO_POS $22F1		; right audio text indicator
.EQU FLASHBOX_POS $214C		; flashing box object

; VRAM Tilemap Sizes
.EQU PAL_SIZE 128			; palette size = 2 bytes for every color, 64 colors (128 = 2 * 64)
.EQU TILEDEF_SIZE 4256		; tile data size = 32 bytes/tile * #of tile entries  (4256 = 32 * 133)
.EQU FULLMAP_SIZE 2048		; full tilemap = 32 * 32 * 2 = 2048
.EQU LOGO_SIZE 170			; logo
.EQU RES_SIZE 14			; screen size text
.EQU SCAN_SIZE 26			; interlace/non-interlace text
.EQU SCAN_SIZE_MAIN 50		; scan-mode text during main test patterns
.EQU LAUDIO_SIZE 26			; left audio indicator
.EQU RAUDIO_SIZE 28         ; right audio indicator

;============================================================================
; Variables
;============================================================================
.ENUM $0000
CurrPattern	DB		; storage for current screen mode
Countdown	DB		; counter for countdown timer
DoFlash		DB      ; keeps track if the flashing object is enabled or not
FlashFrame  DB      ; if yes, this flag tracks if it's on or off during the current frame
TimerText	DW		; pointer to current text character for countdown timer
HelpFlag	DB 		; keeps track on whether help mode is engaged or not
JoyDelay	DB		; storage for accumulated joypad data during delay routine
VideoSet	DB		; variable to track what's in the REG_SETINI register

CurrLineL	DB		; holds current line being drawn
CurrLineH	DB

MapBank		DB		; storage for ROM bank of tile map
MapAdr		DW		; storage for ROM address of tile map
MapPlaceL	DB		; storage for location of where to start drawing the tile map
MapPlaceH	DB
MapSize		DW		; storage for size of tile map

Joy1RawL	DB      ; Holder of RAW joypad data from register (from last frame)
Joy1RawH	DB

Joy1PressL	DB      ; Contains only pressed buttons (not held down)
Joy1PressH	DB
.ENDE

;============================================================================
; Macros
;============================================================================
;----------------------------------------------------------------------------
;============================================================================
; DrawPattern - Draws the referenced one-line tilemap onto the entire screen
;----------------------------------------------------------------------------
; In: 1.)PATTERN -- 24 bit address of tilemap for test pattern
;---------------------------------------------------------------------------- 
; Out: None
;----------------------------------------------------------------------------
; Modifies: A, X, and Y (through DrawLines routine)
; Requires: mem/A = 8 bit, X/Y = 16 bit
;----------------------------------------------------------------------------
; Usage:
; DrawPattern PATTERN
;----------------------------------------------------------------------------
.MACRO DrawPattern
	LDA #:\1		; Bank of tilemap (using a colon before the parameter gets its bank)
	STA MapBank
	LDX #\1			; Tilemap address (not using a colon gets the offset address)
	STX MapAdr
	JSR DrawLines	; Draw the one row tilemap repeatedly to fill the entire screen
.ENDM

;============================================================================
; DrawMap - Draws pre-mapped data at the location specified
;----------------------------------------------------------------------------
; In: 1.) VRAM_START -- Location (word address) in VRAM where to start writing
; In: 2.) TILEMAP_ADDR -- 24 bit address of pre-defined tilemap
; In: 3.) MAP_SIZE -- Size (in bytes) of tilemap
;---------------------------------------------------------------------------- 
; Out: None
;----------------------------------------------------------------------------
; Modifies: A, X, and Y (through DrawLines routine)
; Requires: mem/A = 8 bit, X/Y = 16 bit
;----------------------------------------------------------------------------
; Usage:
; DrawMap VRAM_START, TILEMAP_ADDR, MAP_SIZE
;----------------------------------------------------------------------------
.MACRO DrawMap
	LDX #\1			; word address in VRAM where tilemap should start.  Multiply by two to get the byte address
	STX MapPlaceL
	LDA #:\2		; Bank of tilemap (using a colon before the parameter gets its bank)
	STA MapBank
	LDX #\2			; Tilemap address (not using a colon gets the offset address)
	STX MapAdr
	LDX #\3			; Tilemap size
	STX MapSize
	JSR DrawMapSR
.ENDM

;============================================================================
;============================================================================
; Main Code
;============================================================================
.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"

Start:
    InitSNES			; Clear registers, etc...see "initSNES.inc" for details
	STZ REG_SETINI		; Explicitly initialize SETINI register to zero
	STZ VideoSet		; Initialize tracking variable for SETINI register
	
    ; Load all palette data for our tiles
	STZ REG_CGADD		; Start at first color (zero)
	LDA #:Palette		; Using a colon before the parameter gets its bank.
	LDX #Palette		; Not using a colon gets the offset address.
	LDY #PAL_SIZE.W		; Palette size = 2 bytes for every color, 64 colors
	STX REG_A1T0L		; Store Data offset into DMA source offset
	STA REG_A1B0		; Store data Bank into DMA source bank
	STY REG_DAS0L		; Store size of data block
	STZ REG_DMAP0   	; Set DMA mode (byte, normal increment)
	LDA #$22.B			; Set destination register ($2122 - CGRAM Write)
	STA REG_BBAD0	
	LDA #$01.B			; Initiate DMA transfer (channel 1)
	STA REG_MDMAEN

	; Load all tile data into beginning of VRAM
	LDA #$80.B			; Word VRAM access, increment by 1, no address remapping
	STA REG_VMAIN
	LDX #$0000.W		; Start at beginning of VRAM
	STX REG_VMADDL		; $2116: Word address for accessing VRAM.
	LDA #:Tiles			; Using a colon before the parameter gets its bank.
	LDX #Tiles			; Not using a colon gets the offset address.
	LDY #TILEDEF_SIZE.W	; Tile data size = 32 bytes/tile * #of tile entries
	JSR LoadVRAM		; Perform DMA in subroutine

	; Setup Joypad Input
	LDX #$0000
	STX Joy1RawL		; initialize Joypad1 data storage
	STX Joy1PressL
    STZ REG_JOYSER0		; check if Joypad1 is connected
    LDA #$81.B
    STA REG_NMITIMEN	; enable NMI and auto-joypad read

	; Draw the text for the splash screen
	DrawMap TILEMAP_TOP.W, Splash, FULLMAP_SIZE.W
	
	; Draw the logo for the splash screen
	DrawMap LOGO_START.W, Logo, LOGO_SIZE.W
	
	; Draw Screen Size Text
	LDA VideoSet		; extract out the two screen size bits (bits2&3 of VideoSet)
	BIT #$0C.B			; bit3-horizontal: 0 => 256 1 => 512 ///// bit2-vertical: 0 => 224 1 => 239
	BEQ +				; draw 256x224 if both are zero
	CMP #$0C.B
	BEQ ++				; draw 512x239 if both are one
	CMP #$08.B
	BEQ +++				; draw 512x224 only if horizontal is 1
	DrawMap RESPOS_SPLASH.W, r256x239, RES_SIZE.W	; otherwise draw 256x239
	JMP Splash_scan		; continue to draw scan mode
+:	DrawMap RESPOS_SPLASH.W, r256x224, RES_SIZE.W
	JMP Splash_scan		; continue to draw scan mode
++:	DrawMap RESPOS_SPLASH.W, r512x239, RES_SIZE.W
	JMP Splash_scan		; continue to draw scan mode
+++:DrawMap RESPOS_SPLASH.W, r512x224, RES_SIZE.W

	; Draw Interlace/Non-Interlace Text
Splash_scan:
	LDA VideoSet		; extract out the scan mode bit (bit0 of VideoSet)
	BIT #$01.B			; 0 => noninterlace, 1 => interlace
	BEQ +				; draw corresponding text based on the value
	DrawMap SCANPOS_SPLASH.W, Interlace, SCAN_SIZE.W
	JMP TurnOn
+:	DrawMap SCANPOS_SPLASH.W, Noninterlace, SCAN_SIZE.W
	
	; Setup Video modes, tilemap size, and VRAM offset
	; Then turn on the screen
TurnOn:	
	JSR SetupVideo
	
	; Countdown Timer Loop
	WAI					; Wait for VBlank before beginning
	LDA #5.B
	STA Countdown		; Initialize countdown timer to 5 seconds
	LDX #Timer.W
	STX TimerText		; Initialize text pointer to first character entry
	LDA #$80.B
	STA REG_VMAIN		; Word VRAM access, increment by 1, no address remapping
	LDY #2.W			; single character of text = 2 bytes
	
countdownLoop:
	LDX #TIMER_POS.W	
	STX REG_VMADDL		; reset the position for the counter text
	LDA #:Timer			; use the starting point label to get the bank
	LDX TimerText		; load current timer character
	JSR LoadVRAM		; Perform DMA to update the timer text
	STZ JoyDelay		; reset accumulated joypad data
	LDA #1.B
	JSR DelaySec		; one second delay
	LDA JoyDelay
	BIT #BUTTONH_START	; check if start was pressed during delay
	BNE Setup			; if it was, then begin normal operation
	INC TimerText		; otherwise...
	INC TimerText		; increment the text pointer to the next character (2 bytes)
	DEC Countdown		; decrement counter timer
	BNE countdownLoop	; end if 5 seconds passed, otherwise repeat

	; Initialize the first pattern to be 100% colorbars
Setup:
	WAI					; Wait for VBlank before beginning
	DrawPattern Color	; draw colorbar screen
	STZ CurrPattern		; set pattern tracker to match 100% colorbar index
	STZ HelpFlag		; clear help mode flag
	STZ DoFlash			; no flashing object yet
	STZ FlashFrame      ; initialize on/off flash frame tracker to zero
	
	; Main processing loop.
	; Check for button presses and change display modes accordingly.
mainLoop:
    WAI					; wait for VBlank
	JSR Joypad			; grab joypad data

	LDA HelpFlag		; check if we are in help mode
	BEQ NonHelpJump		; branch around if we are not
	LDA Joy1PressL
	BIT #BUTTON_X		; otherwise check for scan mode change via X button
	BNE SwitchScanJump	; and go handle the change
	LDA Joy1PressH
	BIT #BUTTONH_Y		; or check for screen height change via Y button
	BNE SwitchHeightHelp; and handle it
	BIT #BUTTONH_START	; or check for help mode exit command via START button
	BNE EndHelpJump		; and handle it
	JMP mainLoop		; otherwise repeat the loop

NonHelpJump:
	JMP NonHelp
EndHelpJump:
	JMP EndHelp
SwitchScanJump:
	JMP SwitchScanHelp
	
SwitchHeightHelp:
	DrawPattern Black	; clear the screen of tile data from previous screen height setting
	LDA VideoSet		; load dummy variable which contains the screen height setting
	EOR #$04.B			; toggle vertical height (bit2)
	STA REG_SETINI		; send the update to the video processor
	STA VideoSet		; and store the changes in memory
	JSR DrawHelp
	LDA VideoSet		; extract out the two screen size bits (bits2&3 of VideoSet)
	BIT #$0C.B			; bit3-horizontal: 0 => 256 1 => 512 ///// bit2-vertical: 0 => 224 1 => 239
	BEQ +				; draw 256x224 if both are zero
	CMP #$0C.B
	BEQ ++				; draw 512x239 if both are one
	CMP #$08.B
	BEQ +++				; draw 512x224 only if horizontal is 1
	DrawMap RESPOS_HELP.W, r256x239, RES_SIZE.W	; otherwise draw 256x239
	JMP mainLoop		; continue to draw scan mode
+:	DrawMap RESPOS_HELP.W, r256x224, RES_SIZE.W
	JMP mainLoop		; continue to draw scan mode
++:	DrawMap RESPOS_HELP.W, r512x239, RES_SIZE.W
	JMP mainLoop		; continue to draw scan mode
+++:DrawMap RESPOS_HELP.W, r512x224, RES_SIZE.W
	JMP mainLoop

SwitchScanHelp:
	LDA VideoSet		; load dummy variable which contains the scan mode setting
	EOR #$01.B			; toggle scan mode (bit0)
	STA REG_SETINI		; send the update to the video processor
	STA VideoSet		; and store the changes in memory
	BIT #$01.B			; 0 => noninterlace, 1 => interlace
	BEQ +				; redraw the corresponding text and repeat the loop
	DrawMap SCANPOS_HELP.W, Interlace, SCAN_SIZE.W
	JMP mainLoop
+:	DrawMap SCANPOS_HELP.W, Noninterlace, SCAN_SIZE.W
	JMP mainLoop
	
EndHelp:
	STZ HelpFlag		; clear help flag indicating we are no longer in help mode
	JMP Redraw			; jump down to redraw the current test pattern

NonHelp:
	LDA Joy1PressL
	BIT #BUTTON_X		; check if X button is pressed
	BNE SwitchScan		; if yes, then toggle the scan mode (interlace/non-interlace)
	BIT #BUTTON_A		; check if A button is pressed
	BNE NextPattern		; if yes, then switch to next video mode

	LDA Joy1PressH
	BIT #BUTTONH_Y		; check if Y button is pressed
	BNE ChangeHeightJump; if yes, then change screen height
	BIT #BUTTONH_B		; check if B button is pressed
	BNE PrevPattern		; if yes, then switch to previous video mode
	BIT #BUTTONH_UP     ; check if D-PAD Up button is pressed
	BNE ToggleFlash     ; if yes, then toggle flashing object on/off
	BIT #BUTTONH_LEFT	; check if D-PAD Left button is pressed
	BNE LeftAudioJump	; if yes, then play left channel audio test
	BIT #BUTTONH_RIGHT	; check if D-PAD Right button is pressed
	BNE RightAudioJump	; if yes, then play right channel audio test
	BIT #BUTTONH_START	; check if START button is pressed
	BEQ +               ; if not, then check if flashing object is enabled
	JSR DrawHelp		; if it was, then draw the help screen
	INC HelpFlag		; and track the mode was entered by setting the help flag
	JMP ++
+:	LDA DoFlash         ; is flashing object enabled?
	BNE Redraw          ; if yes, we better go to the drawing loop, otherwise
++:	JMP mainLoop		; repeat the main loop
	
ToggleFlash:
	LDA DoFlash			; load flashing object tracking flag
	EOR #$01.B			; toggle it
	STA DoFlash			; and store it back into memory
	JMP Redraw			; go to drawing loop
	
NextPattern:
	LDA CurrPattern
	INC A				; increment the tracker for the current pattern
	CMP #$0A.B			; check if we've blown past the last pattern
	BNE +				; if not, then continue
	LDA #$00.B			; otherwise set the pattern to zero
+:	STA CurrPattern		; and store the new current pattern setting
	JMP Redraw			; and redraw the screen
	
PrevPattern:
	LDA CurrPattern
	DEC A				; decrement the tracker for the current pattern
	CMP #$FF.B			; check if we've blown past the first pattern
	BNE +				; if not, then continue
	LDA #$09.B          ; otherwise set the pattern to the last one
+:	STA CurrPattern     ; and store the new current pattern setting
	JMP Redraw          ; and redraw the screen

LeftAudioJump:
	JMP LeftAudio
RightAudioJump:
	JMP RightAudio
ChangeHeightJump:
	JMP ChangeHeight

Redraw:
	LDA DoFlash			; load flashing object flag and check if we're supposed to flash
	BEQ +	 			; if not, just redraw the pattern
	LDA FlashFrame		; otherwise, load up if this is an on/off frame
	EOR #$01.B			; flip it for the next time
	STA FlashFrame		; and store it back
	BEQ +				; then decide whether to erase the object
	JSR DrawBox			; or to draw it
	JMP ++
+:	JSR PatternSelect	; draws the current pattern
++:	JMP mainLoop		; and then repeat the main loop
	
SwitchScan:
	JSR PatternSelect
	LDA VideoSet		; load register which contains scan mode setting
	EOR #$01.B			; toggle scan mode (bit0)
	STA REG_SETINI		; send the update to the video processor
	STA VideoSet		; and store the changes in memory
	BIT #$01.B			; 0 => noninterlace, 1 => interlace
	BEQ +				; redraw the corresponding text on the screen
	DrawMap SCANPOS_MAIN.W, Scanmode_interlace, SCAN_SIZE_MAIN.W
	JMP ++
+:	DrawMap SCANPOS_MAIN.W, Scanmode_noninterlace, SCAN_SIZE_MAIN.W
++:	LDA #1.B
	JSR DelaySec		; delay for one second and then redraw entire screen to remove text
	JMP Redraw
	
ChangeHeight:
	JSR PatternSelect
	LDA VideoSet		; load register which contains screen height setting
	EOR #$04.B			; toggle height (bit2)
	STA REG_SETINI		; send the update to the video processor
	STA VideoSet		; and store the changes in memory
	BIT #$0C.B			; bit3-horizontal: 0 => 256 1 => 512 ///// bit2-vertical: 0 => 224 1 => 239
	BEQ +				; draw 256x224 if both are zero
	CMP #$0C.B
	BEQ ++				; draw 512x239 if both are one
	CMP #$08.B
	BEQ +++				; draw 512x224 only if horizontal is 1
	DrawMap RESPOS_MAIN.W, r256x239, RES_SIZE.W	; otherwise draw 256x239
	LDA #1.B
	JSR DelaySec
	JMP Redraw
+:	DrawMap RESPOS_MAIN.W, r256x224, RES_SIZE.W
	LDA #1.B
	JSR DelaySec
	JMP Redraw
++:	DrawMap RESPOS_MAIN.W, r512x239, RES_SIZE.W
	LDA #1.B
	JSR DelaySec
	JMP Redraw
+++:DrawMap RESPOS_MAIN.W, r512x224, RES_SIZE.W
	LDA #1.B
	JSR DelaySec
	JMP Redraw

LeftAudio:
	JSR PlaySoundL		; play left channel sound effect
	WAI					; wait for VBlank and then draw the indicator on the screen
	JSR PatternSelect
	DrawMap LAUDIO_POS.W, LeftAud, LAUDIO_SIZE.W
	LDA #1.B
	JSR DelaySec		; delay for one second
	JMP Redraw			; and then redraw entire screen to remove the indicator

RightAudio:
	JSR PlaySoundR		; play right channel sound effect
	WAI                 ; wait for VBlank and then draw the indicator on the screen
	JSR PatternSelect
	DrawMap RAUDIO_POS.W, RightAud, RAUDIO_SIZE.W
	LDA #1.B
	JSR DelaySec		; delay for one second
	JMP Redraw          ; and then redraw entire screen to remove the indicator
;============================================================================

;============================================================================
; Routines
;============================================================================
;----------------------------------------------------------------------------
;============================================================================
; PatternSelect -- Selects the correct pattern to draw and draws it
;----------------------------------------------------------------------------
; In: None
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: A, X, and Y
;----------------------------------------------------------------------------
; Notes:  Uses the value in CurrPattern to make the decision.
;		  Gets called from the main loop.
;----------------------------------------------------------------------------
PatternSelect:
	LDA CurrPattern
	BEQ DrawColor		; draw 100% colorbars if current pattern = 0
	CMP #$01.B
	BEQ DrawColor75		; draw 75% (74.19%) colorbars if current pattern = 1
	CMP #$02.B
	BEQ DrawGray		; draw graybars if current pattern = 2
	CMP #$03.B
	BEQ DrawWhite		; draw white screen if current pattern = 3
	CMP #$04.B
	BEQ DrawGreen		; draw green screen if current pattern = 4
	CMP #$05.B
	BEQ DrawMagenta		; draw magenta screen if current pattern = 5
	CMP #$06.B
	BEQ DrawBlue		; draw blue screen if current pattern = 6
	CMP #$07.B
	BEQ DrawBlack		; draw black screen if current pattern = 7
	CMP #$08.B
	BEQ DrawSlow		; draw slow bandwidth screen if current pattern = 8
	CMP #$09.B
	JMP DrawFast		; draw fast bandwidth screen if current pattern = 9

DrawColor:
	DrawPattern Color	; draw 100% colorbars
	JMP EndDraw			; and jump to the end of routine
DrawColor75:
	DrawPattern Color75	; draw 75% (74.19%) colorbars
	JMP EndDraw			; and jump to the end of routine
DrawGray:
	DrawPattern Gray	; draw graybars
	JMP EndDraw			; and jump to the end of routine
DrawWhite:
	DrawPattern White	; draw white screen
	JMP EndDraw			; and jump to the end of routine
DrawGreen:
	DrawPattern Green	; draw white screen
	JMP EndDraw			; and jump to the end of routine
DrawMagenta:
	DrawPattern Magenta	; draw magenta screen
	JMP EndDraw			; and jump to the end of routine
DrawBlue:
	DrawPattern Blue	; draw blue screen
	JMP EndDraw			; and jump to the end of routine
DrawBlack:
	DrawPattern Black	; draw black screen
	JMP EndDraw			; and jump to the end of routine
DrawSlow:
	DrawPattern SlowBand	; draw slow bandwidth test pattern
	JMP EndDraw				; and jump to the end of routine
DrawFast:
	DrawPattern FastBand	; draw fast bandwidth test pattern
	JMP EndDraw				; and jump to the end of routine
EndDraw:
	RTS		; return to main loop
;============================================================================
	
;============================================================================
; DelaySec -- Generic Delay Routine
;----------------------------------------------------------------------------
; In: A  -- numbers of seconds to delay by
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: A, X
;----------------------------------------------------------------------------
; Notes:  Load the number of seconds into A before calling
;----------------------------------------------------------------------------
DelaySec:
	LDX #60.W
OneSec:
	WAI		; waiting for VBlank takes approximately 1/60 of a second
	
		; extra code to provide a hook to detect button presses during the delay routine
		PHA				; push the amount of seconds to delay onto the stack
		JSR Joypad		; poll the joypads
		LDA JoyDelay	; load accumulated joypad data
		ORA Joy1PressH	; detect any new presses
		STA JoyDelay	; and save them
		PLA				; recover number of delay seconds from the stack
	
	; continue delay routine
	DEX				
	BNE OneSec		; repeat 60 times to achieve one second of delay
	DEC A
	BNE DelaySec	; repeat the entire routine for the specified number of seconds
	RTS
;============================================================================

;============================================================================
; LoadVRAM -- Load data into VRAM
;----------------------------------------------------------------------------
; In: A:X  -- points to the data
;     Y    -- Number of bytes to copy (0 to 65535)  (assumes 16-bit index)
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: A
;----------------------------------------------------------------------------
; Notes:  Assumes VRAM address has been previously set!!
;----------------------------------------------------------------------------
LoadVRAM:
	STA REG_A1B0	; Store data Bank into DMA source bank
	STX REG_A1T0L	; Store Data offset into DMA source offset
	STY REG_DAS0L	; Store size of data block

	LDA #$01.B
	STA REG_DMAP0   ; Set DMA mode (word, normal increment)
	LDA #$18.B
	STA REG_BBAD0	; Set the destination register (VRAM write register)
	LDA #$01.B
	STA REG_MDMAEN	; Initiate DMA transfer (channel 1)

	RTS				; Return from subroutine
;============================================================================

;============================================================================
; DrawHelp -- Displays the help screen which has operating instructions
;----------------------------------------------------------------------------
; In: None
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: A, X, and Y
;----------------------------------------------------------------------------
; Notes:  Once in help screen, the only valid buttons are
;		  X: for changing the scan mode
;		  START: for exiting help
;----------------------------------------------------------------------------
DrawHelp:
	DrawPattern Black	; first clear the screen of erroneous tile data
	
	; Draw the text for the splash screen
	DrawMap TILEMAP_TOP.W, Help, FULLMAP_SIZE.W
	
	; Draw Screen Size Text
	LDA VideoSet		; extract out the two screen size bits (bits2&3 of VideoSet)
	BIT #$0C.B			; bit3-horizontal: 0 => 256 1 => 512 ///// bit2-vertical: 0 => 224 1 => 239
	BEQ +				; draw 256x224 if both are zero
	CMP #$0C.B
	BEQ ++				; draw 512x239 if both are one
	CMP #$08.B
	BEQ +++				; draw 512x224 only if horizontal is 1
	DrawMap RESPOS_HELP.W, r256x239, RES_SIZE.W	; otherwise draw 256x239
	JMP Help_scan		; continue to draw scan mode
+:	DrawMap RESPOS_HELP.W, r256x224, RES_SIZE.W
	JMP Help_scan		; continue to draw scan mode
++:	DrawMap RESPOS_HELP.W, r512x239, RES_SIZE.W
	JMP Help_scan		; continue to draw scan mode
+++:DrawMap RESPOS_HELP.W, r512x224, RES_SIZE.W

	; Draw Interlace/Non-Interlace Text
Help_scan:	
	LDA VideoSet		; extract out the scan mode bit (bit0 of VideoSet)
	BIT #$01.B			; 0 => noninterlace, 1 => interlace
	BEQ +				; draw corresponding text based on the value
	DrawMap SCANPOS_HELP.W, Interlace, SCAN_SIZE.W
	JMP ++
+:	DrawMap SCANPOS_HELP.W, Noninterlace, SCAN_SIZE.W
++:	RTS
;============================================================================

;============================================================================
; DrawMapSR -- Draws any generic tilemap onto the screen at any location
;----------------------------------------------------------------------------
; In: None
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Notes:  This gets called by the DrawMap MACRO
;----------------------------------------------------------------------------
DrawMapSR:
	LDA #$80.B
	STA REG_VMAIN	; Word VRAM access, increment by 1, no address remapping

	LDA VideoSet	; extract out the screen height bit (bit2 of VideoSet)
	BIT #$04.B		; 0 => 224, 1 => 239
	BEQ +			; if 224, then no vertical offset correction required
	LDA #$20.B		; otherwise
	CLC				; clear carry
	ADC MapPlaceL	; add a line of tiles for offset correction
	STA MapPlaceL	; store it
	LDA MapPlaceH	; also
	ADC #$00.B		; add any carry to the high byte	
	STA MapPlaceH	; and store that too
+:	LDX MapPlaceL	; load tilemap starting location
	STX REG_VMADDL	; $2116: Word address for accessing VRAM.
	LDA MapBank		; load bank of tilemap
	LDX MapAdr		; load tilemap address
	LDY MapSize		; load size of tilemap
	WAI				; wait for vblank to not conflict with DMA
	JSR LoadVRAM	; then perform DMA for this map data
	RTS				; and return when done
;============================================================================

;============================================================================
; DrawBox -- Uses an 8 tile tilemap to make a box
;----------------------------------------------------------------------------
; In: None
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Notes:  Box is also 8 tiles tall
;----------------------------------------------------------------------------
DrawBox:
	LDA #:FlashBox		; Bank of tilemap (using a colon before the parameter gets its bank)
	STA MapBank
	LDX #FlashBox		; Tilemap address (not using a colon gets the offset address)
	STX MapAdr
	LDX #FLASHBOX_POS.W	; top-left corner of the flashing box
	STX CurrLineL		; set it to current line
	LDA #$80.B
	STA REG_VMAIN		; Word VRAM access, increment by 1, no address remapping
	
	LDA VideoSet	; extract out the screen height bit (bit2 of VideoSet)
	BIT #$04.B		; 0 => 224, 1 => 239
	BEQ _BoxLoop	; if 224, then no vertical offset correction required
	LDA #$20.B		; otherwise
	CLC				; clear carry
	ADC CurrLineL	; add a line of tiles for offset correction
	STA CurrLineL	; store it
	LDA CurrLineH	; also
	ADC #$00.B		; add any carry to the high byte	
	STA CurrLineH	; and store that too						
_BoxLoop:
	LDX CurrLineL	; get current line
	STX REG_VMADDL	; $2116: Word address for accessing VRAM.
	LDA MapBank		; Bank of box tilemap
	LDX MapAdr		; Offset address of box tilemap
	LDY #16.W		; Width of flashing box in terms of bytes (bytes = 2*tiles)
	JSR LoadVRAM	; Perform DMA for one line
	LDA CurrLineL	; get current line number
	CLC				; clear carry before adding
	ADC #$20.B		; move address to start of next line
	STA CurrLineL	; store new address
	LDA CurrLineH
	ADC #$00.B		; add any carry to the high byte
	STA CurrLineH	; and store it
	CMP #$22.B		; check if all 8 box lines have been loaded (high-byte)
	BNE _BoxLoop	; repeat if there are more box lines to do
	LDA VideoSet	; otherwise, extract out the screen height bit (bit2 of VideoSet)
	BIT #$04.B      ; 0 => 224, 1 => 239
	BEQ +           ; if 224, then no vertical offset correction required
	LDA CurrLineL	; otherwise
	CMP #$6C.B		; check if all 8 box lines have been loaded (low-byte) using offset correction
	BEQ ++			; and end the routine if it is
	JMP _BoxLoop	; or repeat if there are more box lines to do
+:	LDA CurrLineL
	CMP #$4C.B		; check if all 8 box lines have been loaded (low-byte)
	BNE _BoxLoop	; repeat if there are more box lines to do
++:	RTS				; return if all 8 box lines are done
;============================================================================

;============================================================================
; DrawLines -- Uses a four row tilemap to fill the entire screen
;----------------------------------------------------------------------------
; In: None
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Notes:  32 lines of 32 tiles for a full screen (32x32 tiles & 8x8 pixels ==> 256x256)
;----------------------------------------------------------------------------
DrawLines:
	LDX #TILEMAP_TOP.W	; start of the line
	STX CurrLineL		; set it to current line
	LDA #$80.B
	STA REG_VMAIN	; Word VRAM access, increment by 1, no address remapping
_LineLoop:
	LDX CurrLineL	; get current line
	STX REG_VMADDL	; $2116: Word address for accessing VRAM.
	LDA MapBank		; Bank of test pattern map
	LDX MapAdr		; Offset address of test pattern map
	LDY #256.W		; Size of four lines in terms of bytes (bytes = 2*tiles)
	JSR LoadVRAM	; Perform DMA for four lines
	LDA CurrLineL	; get current line number
	CLC				; clear carry before adding
	ADC #$80.B		; move address to start of the next four lines
	STA CurrLineL	; store new address
	LDA CurrLineH
	ADC #$00.B		; add any carry to the high byte
	STA CurrLineH	; and store it
	CMP #$24.B		; check if all 32 lines have been loaded
	BNE _LineLoop	; repeat if there are more lines to do
	RTS				; or return if all 32 lines are done
;============================================================================

;============================================================================
; SetupVideo -- Sets up the video mode and tile-related registers
;----------------------------------------------------------------------------
; In: None
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
SetupVideo:
	LDA #BGMODE_1.B
	STA REG_BGMODE		; Set Video BG Mode to 1, 8x8 tiles, BG1/BG2 = 16 colors, BG3 = 4 colors, BG4 = N/A

	LDA #$20.B			; Set BG1's Tile Map offset to $2000 (Word address) in order to fit our tile data before that
	STA REG_BG1SC		; and also sets the Tile Map size to 32x32

	STZ REG_BG12NBA		; Set BG1's Character VRAM offset to $0000 (word address)

	LDA #TM_BG1.B
	STA REG_TM			; Enable BG1, leave all others disabled

	LDA #$0F.B
	STA REG_INIDISP		; Turn on screen, full Brightness, forced blanking is OFF

	RTS
;============================================================================

;============================================================================
; VBlank -- Vertical Blanking Interrupt Routine
;----------------------------------------------------------------------------
VBlank:
	PHA
	PHX
	PHY
	PHP				; Save register states

	LDA REG_RDNMI	; Clear NMI flag

	PLP
	PLY
	PLX
	PLA				; Restore register states

    RTI				; Return from interrupt
;============================================================================
.ENDS

;============================================================================
; Character Data (palettes, tiles, tilemaps)
;============================================================================
.BANK 1 SLOT 0
.ORG 0
.SECTION "CharacterData"

	.INCLUDE "graphics.inc"

.ENDS
