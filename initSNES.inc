;============================================================================
; SNES Hardware Initialization
;============================================================================
;----------------------------------------------------------------------------
;============================================================================
;----------------------------------------------------------------------------
; InitSNES -- standard initialization of SNES memory and registers
;----------------------------------------------------------------------------
.MACRO InitSNES
  SEI			; disable interrupts
  CLC			; switch to native mode
  XCE

  REP #$38		; mem/A = 16 bit, X/Y = 16 bit
				; decimal mode off

  LDX #$1FFF	; Setup the stack
  TXS			; Transfer Index X to Stack Pointer Register

  ; do the rest of the initialization in a routine (can do loops in routines)
  JSL $008000	; InitializeSNES starting address

  SEP #$20		; mem/A = 8 bit
  REP #$D3		; clear N,V,C,Z status bits, X/Y = 16 bit
.ENDM

;----------------------------------------------------------------------------

.BANK 0 SLOT 0
.ORG 0
.SECTION "InitializeSNESCode" FORCE

InitializeSNES:
  PHK				; set Data Bank = Program Bank
  PLB

  REP #$30			; mem/A = 16 bit, X/Y = 16 bit

  LDA #$0000		; set Direct Page = $0000
  TCD				; Transfer Accumulator to Direct Register

  SEP #$20			; mem/A = 8 bit
  REP #$10			; X/Y = 16 bit

  LDX $1FFD			; we clear all the mem at one point ...
  STX REG_A1T7L		; so save the return address in a place that won't get overwritten
  LDA $1FFF
  STA REG_A1B7

  LDA #$8F
  STA REG_INIDISP	; turn screen off for now, set brightness to normal

  LDX #REG_OBSEL
_Loop00:			; regs $2101-$210C
  STZ $00,X			; set Sprite,Character,Tile sizes to lowest, and set addresses to $0000
  INX
  CPX #REG_BG1HOFS
  BNE _Loop00

_Loop01:			; regs $210D-$2114
  STZ $00,X			; Set all BG scroll values to $0000
  STZ $00,X			; "write-twice" registers
  INX
  CPX #REG_VMAIN
  BNE _Loop01

  LDA #$80			; reg $2115
  STA REG_VMAIN		; Initialize VRAM transfer mode to word-access, increment by 1

  STZ REG_VMADDL	; regs $2116-$2117
  STZ REG_VMADDH	; VRAM address = $0000

  					; reg $2118-$2119
					; VRAM write register... don't need to initialize


  STZ REG_M7SEL		; clear Mode7 setting

  LDX #REG_M7A
_Loop02:			; regs $211B-$2120
  STZ $00,X			; clear out the Mode7 matrix values
  STZ $00,X			; "write-twice" registers
  INX
  CPX #REG_CGADD
  BNE _Loop02

					; reg $2121 - Color address, doesn't need initilaizing
					; reg $2122 - Color data, is initialized later

  LDX #REG_W12SEL
_Loop03:			; regs $2123-$2133
  STZ $00,X			; turn off windows, main screens, sub screens, color addition,
  INX				; fixed color = $00, no super-impose (external synchronization),
  CPX #REG_MPYL		; no interlaced mode, normal resolution
  BNE _Loop03

					; regs $2134-$2136  - multiplication result, no initialization needed
					; reg $2137 - software H/V latch, no initialization needed
					; reg $2138 - Sprite data read, no initialization needed
					; regs $2139-$213A  - VRAM data read, no initialization needed
					; reg $213B - Color RAM data read, no initialization needed
					; regs $213C-$213D  - H/V latched data read, no initialization needed

  STZ REG_STAT77	; reg $213E - might not be necesary, but selects PPU master/slave mode
					; reg $213F - PPU status flag, no initialization needed

					; reg $2140-$2143 - APU communication regs, no initialization required

					; reg $2180  -  read/write WRAM register, no initialization required
					; reg $2181-$2183  -  WRAM address, no initialization required

					; reg $4016-$4017  - serial JoyPad read registers, no need to initialize


  STZ REG_NMITIMEN	; reg $4200  - disable timers, NMI,and auto-joyread

  LDA #$FF
  STA REG_WRIO		; reg $4201  - programmable I/O write port, initalize to allow reading at in-port

					; regs $4202-$4203  - multiplication registers, no initialization required
					; regs $4204-$4206  - division registers, no initialization required

					; regs $4207-$4208  - Horizontal-IRQ timer setting, since we disabled this, it is OK to not init
					; regs $4209-$420A  - Vertical-IRQ timer setting, since we disabled this, it is OK to not init

  STZ REG_MDMAEN	; reg $420B  - turn off all general DMA channels
  STZ REG_HDMAEN	; reg $420C  - turn off all H-MA channels

  STZ REG_MEMSEL	; reg $420D  - ROM access time to slow (2.68Mhz)

  LDA REG_RDNMI		; reg $4210  - NMI status, reading resets the flag

					; reg $4211  - IRQ status, no need to initialize
					; reg $4212  - H/V blank and JoyRead status, no need to initialize
					; reg $4213  - programmable I/O inport, no need to initialize

					; reg $4214-$4215  - divide results, no need to initialize
					; reg $4216-$4217  - multiplication or remainder results, no need to initialize

					; regs $4218-$421f  - JoyPad read registers, no need to initialize

					; regs $4300-$437F
					; no need to intialize because DMA was disabled above
					; also, we're not sure what all of the registers do, so it is better to leave them at
					; their reset state value

  JSR ClearVRAM		; Reset VRAM
  JSR ClearPalette	; Reset colors

  ;**** Clear Sprite Tables ********

  STZ REG_OAMADDL	; sprites initialized to be off the screen, palette 0, character 0
  STZ REG_OAMADDH

  LDX #$0080		; loop 128 times
  LDA #$F0			; A = 240
_Loop08:
  STA REG_OAMDATA	; set X = 240
  STA REG_OAMDATA	; set Y = 240
  STZ REG_OAMDATA	; set character = $00
  STZ REG_OAMDATA	; set priority=0, no flips
  DEX
  BNE _Loop08

  LDX #$0020		; loop 32 times
_Loop09:
  STZ REG_OAMDATA	; set size bit=0, x MSB = 0
  DEX
  BNE _Loop09

  ;**** Clear WRAM ********

  STZ REG_WMADDL	; set WRAM address to $000000
  STZ REG_WMADDM
  STZ REG_WMADDH

  LDX #$8008		; set DMA mode to fixed source
  STX REG_DMAP0		; 1 reg write once (BYTE) to $2180
  LDX #WRAM_fill_byte
  STX REG_A1T0L		; set source offset
  LDA #:WRAM_fill_byte
  STA REG_A1B0		; set source bank
  LDX #$0000
  STX REG_DAS0L		; set transfer size to 64k bytes
  LDA #$01
  STA REG_MDMAEN	; initiate transfer

  LDA #$01          ; now set the next 64k bytes
  STA REG_MDMAEN	; initiate transfer

  PHK				; make sure Data Bank = Program Bank
  PLB

  CLI				; enable interrupts again

  LDX REG_A1T7L		; get our return address...
  STX $1FFD
  LDA REG_A1B7
  STA $1FFF
  RTL				; done with subroutine

WRAM_fill_byte:
  .DB $00

;----------------------------------------------------------------------------
; ClearVRAM -- Sets every byte of VRAM to zero
; In: None
; Out: None
; Modifies: flags
;----------------------------------------------------------------------------
ClearVRAM:
   LDA #$80
   STA REG_VMAIN	; set VRAM port to word access
   LDX #$1809		; set DMA mode to fixed source
   STX REG_DMAP0	; 2 regs write once (WORD) to $2118/9
   LDX #$0000
   STX REG_VMADDL	; set VRAM port address to $0000
   STX $0000		; set $00:0000 to $0000 (assumes scratchpad ram)
   STX REG_A1T0L	; set source address to $xx:0000
   STZ REG_A1B0		; set source bank to $00
   LDX #$FFFF
   STX REG_DAS0L	; set transfer size to 64k-1 bytes
   LDA #$01
   STA REG_MDMAEN	; initiate transfer

   STZ REG_VMDATAH	; clear the last byte of the VRAM

   RTS
;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
; ClearPalette -- Reset all palette colors to zero
; In: None
; Out: None
; Modifies: flags
;----------------------------------------------------------------------------
ClearPalette:
   STZ REG_CGADD
   LDX #$0100		; 256 colors in the palette
ClearPaletteLoop:
   STZ REG_CGDATA	; clear out palette data
   STZ REG_CGDATA	; "write-twice" registers
   DEX
   BNE ClearPaletteLoop

   RTS
;----------------------------------------------------------------------------
.ENDS
