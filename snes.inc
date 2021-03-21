;==================================================================================;
; SNES Definitions of Registers & Constants
;==================================================================================;
;----------------------------------------------------------------------------------;
;==================================================================================;
; Registers
;==================================================================================;
.EQU REG_INIDISP	$2100	; Screen Display Register					1B/W
.EQU REG_OBSEL		$2101	; OAM Size Control							1B/W
.EQU REG_OAMADDL	$2102	; OAM Access Address (Low)					2B/W
.EQU REG_OAMADDH	$2103	; OAM Access Address (High)					(1B/W)
.EQU REG_OAMDATA	$2104	; OAM Data Write							1B/W
.EQU REG_BGMODE		$2105	; Screen Mode Register						1B/W
.EQU REG_MOSAIC		$2106	; Screen Pixelation Register				1B/W
.EQU REG_BG1SC		$2107	; BG1 VRAM Location Register				1B/W
.EQU REG_BG2SC		$2108	; BG2 VRAM Location Register				1B/W
.EQU REG_BG3SC		$2109	; BG3 VRAM Location Register				1B/W
.EQU REG_BG4SC		$210A	; BG4 VRAM Location Register				1B/W
.EQU REG_BG12NBA	$210B	; BG1/2 Character Bases						1B/W
.EQU REG_BG34NBA	$210C	; BG3/4 Character Bases						1B/W
.EQU REG_BG1HOFS	$210D	; BG1 Horizontal Scroll						1B/W D
.EQU REG_BG1VOFS	$210E	; BG1 Vertical Scroll						1B/W D
.EQU REG_BG2HOFS	$210F	; BG2 Horizontal Scroll						1B/W D
.EQU REG_BG2VOFS	$2110	; BG2 Vertical Scroll						1B/W D
.EQU REG_BG3HOFS	$2111	; BG3 Horizontal Scroll						1B/W D
.EQU REG_BG3VOFS	$2112	; BG3 Vertical Scroll						1B/W D
.EQU REG_BG4HOFS	$2113	; BG4 Horiztonal Scroll						1B/W D
.EQU REG_BG4VOFS	$2114	; BG4 Vertical Scroll						1B/W D
.EQU REG_VMAIN		$2115	; Video Port Control						1B/W
.EQU REG_VMADDL		$2116	; Video Port Address (Low)					2B/W
.EQU REG_VMADDH		$2117	; Video Port Address (High)					(1B/W)
.EQU REG_VMDATAL	$2118	; Video Port Data (Low)						2B/W
.EQU REG_VMDATAH	$2119	; Video Port Data (High)					(1B/W)
.EQU REG_M7SEL		$211A	; MODE7 settings register					1B/W
.EQU REG_M7A		$211B	; MODE7 COSINE A							1B/W
.EQU REG_M7B		$211C	; MODE7 SINE A								1B/W
.EQU REG_M7C		$211D	; MODE7 SINE B								1B/W
.EQU REG_M7D		$211E	; MODE7 COSINE B							1B/W
.EQU REG_M7X		$211F	; MODE7 Center Pos X						1B/W D
.EQU REG_M7Y		$2120	; MODE7 Center Pos Y						1B/W D
.EQU REG_CGADD		$2121	; CGRAM Address								1B/W
.EQU REG_CGDATA		$2122	; CGRAM Data Write							1B/W D
.EQU REG_W12SEL		$2123	; Window Mask Settings Reg1					1B/W
.EQU REG_W34SEL		$2124	; Window Mask Settings Reg2					1B/W
.EQU REG_WOBJSEL	$2125	; Window Mask Settings Reg3					1B/W
.EQU REG_WH0		$2126	; Window 1 Left Posision Reg				1B/W
.EQU REG_WH1		$2127	; Window 1 Right Position Reg				1B/W
.EQU REG_WH2		$2128	; Window 2 Left Position Reg				1B/W
.EQU REG_WH3		$2129	; Window 2 Right Position Reg				1B/W
.EQU REG_WBGLOG		$212A	; Mask Logic for Window 1 & 2				1B/W
.EQU REG_WOBJLOG	$212B	; Mask Logic for Color&OBJ Windows			1B/W
.EQU REG_TM			$212C	; Main Screen Designation					1B/W
.EQU REG_TS			$212D	; Sub-Screen Designation					1B/W
.EQU REG_TMW		$212E	; WinMask Main Designation Reg				1B/W
.EQU REG_TSW		$212F	; WinMask Sub Designation Reg				1B/W
.EQU REG_CGSWSEL	$2130	; Fixed Color/Screen Addition Reg			1B/W
.EQU REG_CGADSUB	$2131	; +/- For Screens/BGs/OBJs					1B/W
.EQU REG_COLDATA	$2132	; Fixed Color Data for +/-					1B/W
.EQU REG_SETINI		$2133	; Screen Mode Select Reg					1B/W
.EQU REG_MPYL		$2134	; Multiplication Result (Low)				3B/R
.EQU REG_MPYM		$2135	; Multiplication Result (Middle)			(1B/R)
.EQU REG_MPYH		$2136	; Multiplication Result (High)				(1B/R)
.EQU REG_SLHV		$2137	; Sofware Latch For H/V Counter				1B/R
.EQU REG_OAMDATAREAD	$2138	; OAM Data Read							1B/R
.EQU REG_VMDATALREAD	$2139	; VRAM Data Read (Low)					1B/R
.EQU REG_VMDATAHREAD	$213A	; VRAM Data Read (High)					1B/R
.EQU REG_CGDATAREAD		$213B	; CGRAM Data Read						1B/R
.EQU REG_OPHCT		$213C	; X Scanline Location						1B/R D
.EQU REG_OPVCT		$213D	; Y Scanline Location						1B/R D
.EQU REG_STAT77		$213E	; PPU Status Flag & Version					1B/R
.EQU REG_STAT78		$213F	; PPU Status Flag & Version					1B/R
.EQU REG_APUIO0		$2140	; Sound Register							1B/RW
.EQU REG_APUIO1		$2141	; Sound Register							1B/RW
.EQU REG_APUIO2		$2142	; Sound Register							1B/RW
.EQU REG_APUIO3		$2143	; Sound Register							1B/RW
.EQU REG_WMDATA		$2180	; WRAM Data Read/Write						1B/RW
.EQU REG_WMADDL		$2181	; WRAM Address (Low)						3B/RW
.EQU REG_WMADDM		$2182	; WRAM Address (Middle)						(1B/RW)
.EQU REG_WMADDH		$2183	; WRAM Address (High)						(1B/RW)

.EQU REG_JOYSER0	$4016	; NES-style Joypad Access Port 1			1B/RW
.EQU REG_JOYSER1	$4017	; NES-style Joypad Access Port 2			1B/R
		
.EQU REG_NMITIMEN	$4200	; Counter Enable							1B/W
.EQU REG_WRIO		$4201	; Programmable I/O Port						1B/W
.EQU REG_WRMPYA		$4202	; Multiplicand								1B/W
.EQU REG_WRMPYB		$4203	; Multiplier								1B/W
.EQU REG_WRDIVL		$4204	; Dividend C (Low)							2B/W
.EQU REG_WRDIVH		$4205	; Dividend C (High)							(1B/W)
.EQU REG_WRDIVB		$4206	; Divisor B									1B/W
.EQU REG_HTIMEL		$4207	; Video X IRQ Beam Pointer	(Low)			2B/W
.EQU REG_HTIMEH		$4208	; Video X IRQ Beam Pointer	(High)			(1B/W)
.EQU REG_VTIMEL		$4209	; Video Y IRQ Beam Pointer	(Low)			2B/W
.EQU REG_VTIMEH		$420A	; Video Y IRQ Beam Pointer	(High)			(1B/W)
.EQU REG_MDMAEN		$420B	; DMA Enable Register						1B/W
.EQU REG_HDMAEN		$420C	; HDMA Enable Register						1B/W
.EQU REG_MEMSEL		$420D	; Cycle Speed Register						1B/W
.EQU REG_RDNMI		$4210	; NMI Register								1B/R
.EQU REG_TIMEUP		$4211	; Video IRQ Register						1B/RW
.EQU REG_HVBJOY		$4212	; Status Register							1B/RW
.EQU REG_RDIO		$4213	; Programmable I/O Port						1B/RW
.EQU REG_RDDIVL		$4214	; Quotient Of Divide Result (Low)			2B/R
.EQU REG_RDDIVH		$4215	; Quotient Of Divide Result (High)			(1B/R)
.EQU REG_RDMPYL		$4216	; Mult Or Divide Result A (Low)				2B/R
.EQU REG_RDMPYH		$4217	; Mult Or Divide Result A (High)			(1B/R)
.EQU REG_JOY1L		$4218	; Joypad #1 Status (Low)					2B/R
.EQU REG_JOY1H		$4219	; Joypad #1 Status (High)					(1B/R)
.EQU REG_JOY2L		$421A	; Joypad #2 Status (Low)					2B/R
.EQU REG_JOY2H		$421B	; Joypad #2 Status (High)					(1B/R)
.EQU REG_JOY3L		$421C	; Joypad #3 Status (Low)					2B/R
.EQU REG_JOY3H		$421D	; Joypad #3 Status (High)					(1B/R)
.EQU REG_JOY4L		$421E	; Joypad #4 Status (Low)					2B/R
.EQU REG_JOY4H		$421F	; Joypad #4 Status (High)					(1B/R)
		
.EQU REG_DMAP0		$4300	; DMA0 Control Register						1B/W
.EQU REG_BBAD0		$4301	; DMA0 Destination Register					1B/W
.EQU REG_A1T0L		$4302	; DMA0 Source Address (Low)					2B/W
.EQU REG_A1T0H		$4303	; DMA0 Source Address (High)				(1B/W)
.EQU REG_A1B0		$4304	; DMA0 Source Bank Address					1B/W
.EQU REG_DAS0L		$4305	; DMA0 Transfer size/HDMA0 Address (Low)	2B/W
.EQU REG_DAS0H		$4306	; DMA0 Transfer size/HDMA0 Address (High)	(1B/W)
.EQU REG_DASB0		$4307	; HDMA0 Indirect Address bank byte			1B/W
.EQU REG_A2A0L		$4308	; HDMA0 Table Address (Low)					2B/W
.EQU REG_A2A0H		$4309	; HDMA0 Table Address (High)				(1B/W)
.EQU REG_NLTR0		$430A	; HDMA0 Number Of Lines						1B/W
		
.EQU REG_DMAP1		$4310	; DMA1 Control Register						1B/W
.EQU REG_BBAD1		$4311	; DMA1 Destination Register					1B/W
.EQU REG_A1T1L		$4312	; DMA1 Source Address (Low)					2B/W
.EQU REG_A1T1H		$4313	; DMA1 Source Address (High)				(1B/W)
.EQU REG_A1B1		$4314	; DMA1 Source Bank Address					1B/W
.EQU REG_DAS1L		$4315	; DMA1 Transfer size/HDMA1 Address (Low)	2B/W
.EQU REG_DAS1H		$4316	; DMA1 Transfer size/HDMA1 Address (High)	(1B/W)
.EQU REG_DASB1		$4317	; HDMA1 Indirect Address bank byte			1B/W
.EQU REG_A2A1L		$4318	; HDMA1 Table Address (Low)					2B/W
.EQU REG_A2A1H		$4319	; HDMA1 Table Address (High)				(1B/W)
.EQU REG_NLTR1		$431A	; HDMA1 Number Of Lines						1B/W
		
.EQU REG_DMAP2		$4320	; DMA2 Control Register						1B/W
.EQU REG_BBAD2		$4321	; DMA2 Destination Register					1B/W
.EQU REG_A1T2L		$4322	; DMA2 Source Address (Low)					2B/W
.EQU REG_A1T2H		$4323	; DMA2 Source Address (High)				(1B/W)
.EQU REG_A1B2		$4324	; DMA2 Source Bank Address					1B/W
.EQU REG_DAS2L		$4325	; DMA2 Transfer size/HDMA2 Address (Low)	2B/W
.EQU REG_DAS2H		$4326	; DMA2 Transfer size/HDMA2 Address (High)	(1B/W)
.EQU REG_DASB2		$4327	; HDMA2 Indirect Address bank byte			1B/W
.EQU REG_A2A2L		$4328	; HDMA2 Table Address (Low)					2B/W
.EQU REG_A2A2H		$4329	; HDMA2 Table Address (High)				(1B/W)
.EQU REG_NLTR2		$432A	; HDMA2 Number Of Lines						1B/W
		
.EQU REG_DMAP3		$4330	; DMA3 Control Register						1B/W
.EQU REG_BBAD3		$4331	; DMA3 Destination Register					1B/W
.EQU REG_A1T3L		$4332	; DMA3 Source Address (Low)					2B/W
.EQU REG_A1T3H		$4333	; DMA3 Source Address (High)				(1B/W)
.EQU REG_A1B3		$4334	; DMA3 Source Bank Address					1B/W
.EQU REG_DAS3L		$4335	; DMA3 Transfer size/HDMA3 Address (Low)	2B/W
.EQU REG_DAS3H		$4336	; DMA3 Transfer size/HDMA3 Address (High)	(1B/W)
.EQU REG_DASB3		$4337	; HDMA3 Indirect Address bank byte			1B/W
.EQU REG_A2A3L		$4338	; HDMA3 Table Address (Low)					2B/W
.EQU REG_A2A3H		$4339	; HDMA3 Table Address (High)				(1B/W)
.EQU REG_NLTR3		$433A	; HDMA3 Number Of Lines						1B/W
		
.EQU REG_DMAP4		$4340	; DMA4 Control Register						1B/W
.EQU REG_BBAD4		$4341	; DMA4 Destination Register					1B/W
.EQU REG_A1T4L		$4342	; DMA4 Source Address (Low)					2B/W
.EQU REG_A1T4H		$4343	; DMA4 Source Address (High)				(1B/W)
.EQU REG_A1B4		$4344	; DMA4 Source Bank Address					1B/W
.EQU REG_DAS4L		$4345	; DMA4 Transfer size/HDMA4 Address (Low)	2B/W
.EQU REG_DAS4H		$4346	; DMA4 Transfer size/HDMA4 Address (High)	(1B/W)
.EQU REG_DASB4		$4347	; HDMA4 Indirect Address bank byte			1B/W
.EQU REG_A2A4L		$4348	; HDMA4 Table Address (Low)					2B/W
.EQU REG_A2A4H		$4349	; HDMA4 Table Address (High)				(1B/W)
.EQU REG_NLTR4		$434A	; HDMA4 Number Of Lines						1B/W
		
.EQU REG_DMAP5		$4350	; DMA5 Control Register						1B/W
.EQU REG_BBAD5		$4351	; DMA5 Destination Register					1B/W
.EQU REG_A1T5L		$4352	; DMA5 Source Address (Low)					2B/W
.EQU REG_A1T5H		$4353	; DMA5 Source Address (High)				(1B/W)
.EQU REG_A1B5		$4354	; DMA5 Source Bank Address					1B/W
.EQU REG_DAS5L		$4355	; DMA5 Transfer size/HDMA5 Address (Low)	2B/W
.EQU REG_DAS5H		$4356	; DMA5 Transfer size/HDMA5 Address (High)	(1B/W)
.EQU REG_DASB5		$4357	; HDMA5 Indirect Address bank byte			1B/W
.EQU REG_A2A5L		$4358	; HDMA5 Table Address (Low)					2B/W
.EQU REG_A2A5H		$4359	; HDMA5 Table Address (High)				(1B/W)
.EQU REG_NLTR5		$435A	; HDMA5 Number Of Lines						1B/W
		
.EQU REG_DMAP6		$4360	; DMA6 Control Register						1B/W
.EQU REG_BBAD6		$4361	; DMA6 Destination Register					1B/W
.EQU REG_A1T6L		$4362	; DMA6 Source Address (Low)					2B/W
.EQU REG_A1T6H		$4363	; DMA6 Source Address (High)				(1B/W)
.EQU REG_A1B6		$4364	; DMA6 Source Bank Address					1B/W
.EQU REG_DAS6L		$4365	; DMA6 Transfer size/HDMA6 Address (Low)	2B/W
.EQU REG_DAS6H		$4366	; DMA6 Transfer size/HDMA6 Address (High)	(1B/W)
.EQU REG_DASB6		$4367	; HDMA6 Indirect Address bank byte			1B/W
.EQU REG_A2A6L		$4368	; HDMA6 Table Address (Low)					2B/W
.EQU REG_A2A6H		$4369	; HDMA6 Table Address (High)				(1B/W)
.EQU REG_NLTR6		$436A	; HDMA6 Number Of Lines						1B/W
		
.EQU REG_DMAP7	 	$4370	; DMA7 Control Register						1B/W
.EQU REG_BBAD7		$4371	; DMA7 Destination Register					1B/W
.EQU REG_A1T7L		$4372	; DMA7 Source Address (Low)					2B/W
.EQU REG_A1T7H		$4373	; DMA7 Source Address (High)				(1B/W)
.EQU REG_A1B7		$4374	; DMA7 Source Bank Address					1B/W
.EQU REG_DAS7L		$4375	; DMA7 Transfer size/HDMA7 Address (Low)	2B/W
.EQU REG_DAS7H		$4376	; DMA7 Transfer size/HDMA7 Address (High)	(1B/W)
.EQU REG_DASB7		$4377	; HDMA7 Indirect Address bank byte			1B/W
.EQU REG_A2A7L		$4378	; HDMA7 Table Address (Low)					2B/W
.EQU REG_A2A7H		$4379	; HDMA7 Table Address (High)				(1B/W)
.EQU REG_NLTR7		$437A	; HDMA7 Number Of Lines						1B/W
		
.EQU REG_FBNANACNT	$FEED	; Felon's Banana Register					1B/RW
;==================================================================================;
; Constants
;==================================================================================;
; Background Modes
.EQU BGMODE_0		$00
.EQU BGMODE_1		$01
.EQU BGMODE_2		$02
.EQU BGMODE_3		$03
.EQU BGMODE_4		$04
.EQU BGMODE_5		$05
.EQU BGMODE_6		$06
.EQU BGMODE_7		$07
.EQU BGMODE_PRIO	$08
.EQU BGMODE_16x16_1	$10
.EQU BGMODE_16x16_2	$20
.EQU BGMODE_16x16_3	$40
.EQU BGMODE_16x16_4	$80

; Main Screen Designations
.EQU TM_BG1			$01
.EQU TM_BG2			$02
.EQU TM_BG3			$04
.EQU TM_BG4			$08
.EQU TM_OBJ			$10

; Joypad Buttons
.EQU BUTTON_R		$10
.EQU BUTTON_L		$20
.EQU BUTTON_X		$40
.EQU BUTTON_A		$80
.EQU BUTTON_RIGHT	$0100
.EQU BUTTONH_RIGHT	$01
.EQU BUTTON_LEFT	$0200
.EQU BUTTONH_LEFT	$02
.EQU BUTTON_DOWN	$0400
.EQU BUTTONH_DOWN	$04
.EQU BUTTON_UP		$0800
.EQU BUTTONH_UP		$08
.EQU BUTTON_START	$1000
.EQU BUTTONH_START	$10
.EQU BUTTON_SELECT	$2000
.EQU BUTTONH_SELECT	$20
.EQU BUTTON_Y		$4000
.EQU BUTTONH_Y		$40
.EQU BUTTON_B		$8000
.EQU BUTTONH_B		$80

; Object Sizes and Character Addresses
.EQU OBSEL_8_16			(0<<5)
.EQU OBSEL_8_32			(1<<5)
.EQU OBSEL_8_64			(2<<5)
.EQU OBSEL_16_32		(3<<5)
.EQU OBSEL_16_64		(4<<5)
.EQU OBSEL_32_64		(5<<5)
.EQU OBSEL_16x32_32x64	(6<<5)
.EQU OBSEL_16x32_32x32	(7<<5)
.EQU OBSEL_NN_16K		(3<<3)
.EQU OBSEL_NN_12K		(2<<3)
.EQU OBSEL_NN_8K		(1<<3)
