;----------------------------SNES Header File -------------------------------;
;----------------------------------------------------------------------------;

;=======LoRom=======;
.MEMORYMAP			; Begin describing the system architecture.
	SLOTSIZE $8000  ; The slot is $8000 bytes in size.
	DEFAULTSLOT 0   ; There's only 1 slot in SNES, there are more in other consoles.
	SLOT 0 $8000    ; Define's Slot 0's starting address.
.ENDME				; End MemoryMap definition
.ROMBANKSIZE $8000	; Every ROM bank is 32 KBytes in size
.ROMBANKS 16		; 4 Mbits - Tell WLA we want to use 16 ROM Banks
;===================;

;=======HiRom=======;
;.MEMORYMAP			; Begin describing the system architecture.
;	SLOTSIZE $10000 ; The slot is $8000 bytes in size.
;	DEFAULTSLOT 0   ; There's only 1 slot in SNES, there are more in other consoles.
;	SLOT 0 $0000    ; Define's Slot 0's starting address.
;.ENDME				; End MemoryMap definition
;.ROMBANKSIZE $10000; Every ROM bank is 64 KBytes in size
;.ROMBANKS 8		; 4 Mbits - Tell WLA we want to use 8 ROM Banks
;===================;

.SNESHEADER
	ID "SNES"						; 1-4 letter string, just leave it as "SNES"

	NAME "HD Retrovision - Test"	; Program Title - can't be over 21 bytes,
	;    "123456789012345678901"	; use spaces for unused bytes of the name.

	SLOWROM
	LOROM

	CARTRIDGETYPE $00				; $00 = ROM only, see WLA documentation for others
	ROMSIZE $09						; $09 = 4 Mbits,  see WLA doc for more..
	SRAMSIZE $00					; No SRAM         see WLA doc for more..
	COUNTRY $01						; $01 = U.S.  $00 = Japan  $02 = Australia, Europe, Oceania and Asia
									; $03 = Sweden  $04 = Finland  $05 = Denmark  $06 = France  $07 = Holland
									; $08 = Spain  $09 = Germany, Austria and Switzerland  $0A = Italy  
									; $0B = Hong Kong and China  $0C = Indonesia  $0D = Korea
	LICENSEECODE $00				; Just use $00
	VERSION $14						; $00 = v0.0, $01 = v0.1, ..., $10 = v1.0, $11 = v1.1, ..., etc.
.ENDSNES

.SNESNATIVEVECTOR					; Define Native Mode interrupt vector table
	COP EmptyHandler
	BRK EmptyHandler
	ABORT EmptyHandler
	NMI VBlank
	IRQ EmptyHandler
.ENDNATIVEVECTOR

.SNESEMUVECTOR						; Define Emulation Mode interrupt vector table
	COP EmptyHandler
	ABORT EmptyHandler
	NMI VBlank
	RESET Start
	IRQBRK EmptyHandler
.ENDEMUVECTOR

.BANK 0 SLOT 0						; Defines the ROM bank and the slot it is inserted in memory.
.ORG 0								; .ORG 0 is really $8000, because the slot starts at $8000
.SECTION "EmptyVectors" SEMIFREE

EmptyHandler:
	RTI

.ENDS

.EMPTYFILL $FF
