;*******************************************************************************
;                                                                              *
;    Microchip licenses this software to you solely for use with Microchip     *
;    products. The software is owned by Microchip and/or its licensors, and is *
;    protected under applicable copyright laws.  All rights reserved.          *
;                                                                              *
;    This software and any accompanying information is for suggestion only.    *
;    It shall not be deemed to modify Microchip?s standard warranty for its    *
;    products.  It is your responsibility to ensure that this software meets   *
;    your requirements.                                                        *
;                                                                              *
;    SOFTWARE IS PROVIDED "AS IS".  MICROCHIP AND ITS LICENSORS EXPRESSLY      *
;    DISCLAIM ANY WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING  *
;    BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS    *
;    FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL          *
;    MICROCHIP OR ITS LICENSORS BE LIABLE FOR ANY INCIDENTAL, SPECIAL,         *
;    INDIRECT OR CONSEQUENTIAL DAMAGES, LOST PROFITS OR LOST DATA, HARM TO     *
;    YOUR EQUIPMENT, COST OF PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR    *
;    SERVICES, ANY CLAIMS BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY   *
;    DEFENSE THEREOF), ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER      *
;    SIMILAR COSTS.                                                            *
;                                                                              *
;    To the fullest extend allowed by law, Microchip and its licensors         *
;    liability shall not exceed the amount of fee, if any, that you have paid  *
;    directly to Microchip to use this software.                               *
;                                                                              *
;    MICROCHIP PROVIDES THIS SOFTWARE CONDITIONALLY UPON YOUR ACCEPTANCE OF    *
;    THESE TERMS.                                                              *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Filename:                                                                 *
;    Date:                                                                     *
;    File Version:                                                             *
;    Author:                                                                   *
;    Company:                                                                  *
;    Description:                                                              *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Notes: In the MPLAB X Help, refer to the MPASM Assembler documentation    *
;    for information on assembly instructions.                                 *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Known Issues: This template is designed for relocatable code.  As such,   *
;    build errors such as "Directive only allowed when generating an object    *
;    file" will result when the 'Build in Absolute Mode' checkbox is selected  *
;    in the project properties.  Designing code in absolute mode is            *
;    antiquated - use relocatable mode.                                        *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Revision History:                                                         *
;                                                                              *
;*******************************************************************************



;*******************************************************************************
; Processor Inclusion
;
; TODO Step #1 Open the task list under Window > Tasks.  Include your
; device .inc file - e.g. #include <device_name>.inc.  Available
; include files are in C:\Program Files\Microchip\MPLABX\mpasmx
; assuming the default installation path for MPLAB X.  You may manually find
; the appropriate include file for your device here and include it, or
; simply copy the include generated by the configuration bits
; generator (see Step #2).
;
;*******************************************************************************

; TODO INSERT INCLUDE CODE HERE
#include "p18f25k40.inc"
;*******************************************************************************
;
; TODO Step #2 - Configuration Word Setup
;
; The 'CONFIG' directive is used to embed the configuration word within the
; .asm file. MPLAB X requires users to embed their configuration words
; into source code.  See the device datasheet for additional information
; on configuration word settings.  Device configuration bits descriptions
; are in C:\Program Files\Microchip\MPLABX\mpasmx\P<device_name>.inc
; (may change depending on your MPLAB X installation directory).
;
; MPLAB X has a feature which generates configuration bits source code.  Go to
; Window > PIC Memory Views > Configuration Bits.  Configure each field as
; needed and select 'Generate Source Code to Output'.  The resulting code which
; appears in the 'Output Window' > 'Config Bits Source' tab may be copied
; below.
;
;*******************************************************************************

; TODO INSERT CONFIG HERE
    
; PIC18F25K40 Configuration Bit Settings

; Assembly source line config statements

; CONFIG1L
  CONFIG  FEXTOSC = OFF         ; External Oscillator mode Selection bits (Oscillator not enabled)
  CONFIG  RSTOSC = HFINTOSC_64MHZ; Power-up default value for COSC bits (HFINTOSC with HFFRQ = 64 MHz and CDIV = 1:1)

; CONFIG1H
  CONFIG  CLKOUTEN = OFF        ; Clock Out Enable bit (CLKOUT function is disabled)
  CONFIG  CSWEN = ON            ; Clock Switch Enable bit (Writing to NOSC and NDIV is allowed)
  CONFIG  FCMEN = ON            ; Fail-Safe Clock Monitor Enable bit (Fail-Safe Clock Monitor enabled)

; CONFIG2L
  CONFIG  MCLRE = EXTMCLR       ; Master Clear Enable bit (If LVP = 0, MCLR pin is MCLR; If LVP = 1, RE3 pin function is MCLR )
  CONFIG  PWRTE = OFF           ; Power-up Timer Enable bit (Power up timer disabled)
  CONFIG  LPBOREN = OFF         ; Low-power BOR enable bit (ULPBOR disabled)
  CONFIG  BOREN = SBORDIS       ; Brown-out Reset Enable bits (Brown-out Reset enabled , SBOREN bit is ignored)

; CONFIG2H
  CONFIG  BORV = VBOR_2P45      ; Brown Out Reset Voltage selection bits (Brown-out Reset Voltage (VBOR) set to 2.45V)
  CONFIG  ZCD = OFF             ; ZCD Disable bit (ZCD disabled. ZCD can be enabled by setting the ZCDSEN bit of ZCDCON)
  CONFIG  PPS1WAY = ON          ; PPSLOCK bit One-Way Set Enable bit (PPSLOCK bit can be cleared and set only once; PPS registers remain locked after one clear/set cycle)
  CONFIG  STVREN = ON           ; Stack Full/Underflow Reset Enable bit (Stack full/underflow will cause Reset)
  CONFIG  DEBUG = OFF           ; Debugger Enable bit (Background debugger disabled)
  CONFIG  XINST = OFF           ; Extended Instruction Set Enable bit (Extended Instruction Set and Indexed Addressing Mode disabled)

; CONFIG3L
  CONFIG  WDTCPS = WDTCPS_31    ; WDT Period Select bits (Divider ratio 1:65536; software control of WDTPS)
  CONFIG  WDTE = OFF            ; WDT operating mode (WDT Disabled)

; CONFIG3H
  CONFIG  WDTCWS = WDTCWS_7     ; WDT Window Select bits (window always open (100%); software control; keyed access not required)
  CONFIG  WDTCCS = SC           ; WDT input clock selector (Software Control)

; CONFIG4L
  CONFIG  WRT0 = OFF            ; Write Protection Block 0 (Block 0 (000800-001FFFh) not write-protected)
  CONFIG  WRT1 = OFF            ; Write Protection Block 1 (Block 1 (002000-003FFFh) not write-protected)
  CONFIG  WRT2 = OFF            ; Write Protection Block 2 (Block 2 (004000-005FFFh) not write-protected)
  CONFIG  WRT3 = OFF            ; Write Protection Block 3 (Block 3 (006000-007FFFh) not write-protected)

; CONFIG4H
  CONFIG  WRTC = OFF            ; Configuration Register Write Protection bit (Configuration registers (300000-30000Bh) not write-protected)
  CONFIG  WRTB = OFF            ; Boot Block Write Protection bit (Boot Block (000000-0007FFh) not write-protected)
  CONFIG  WRTD = OFF            ; Data EEPROM Write Protection bit (Data EEPROM not write-protected)
  CONFIG  SCANE = ON            ; Scanner Enable bit (Scanner module is available for use, SCANMD bit can control the module)
  CONFIG  LVP = OFF             ; Low Voltage Programming Enable bit (HV on MCLR/VPP must be used for programming)

; CONFIG5L
  CONFIG  CP = OFF              ; UserNVM Program Memory Code Protection bit (UserNVM code protection disabled)
  CONFIG  CPD = OFF             ; DataNVM Memory Code Protection bit (DataNVM code protection disabled)

; CONFIG5H

; CONFIG6L
  CONFIG  EBTR0 = OFF           ; Table Read Protection Block 0 (Block 0 (000800-001FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR1 = OFF           ; Table Read Protection Block 1 (Block 1 (002000-003FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR2 = OFF           ; Table Read Protection Block 2 (Block 2 (004000-005FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR3 = OFF           ; Table Read Protection Block 3 (Block 3 (006000-007FFFh) not protected from table reads executed in other blocks)

; CONFIG6H
  CONFIG  EBTRB = OFF           ; Boot Block Table Read Protection bit (Boot Block (000000-0007FFh) not protected from table reads executed in other blocks)




;*******************************************************************************
;
; TODO Step #3 - Variable Definitions
;
; Refer to datasheet for available data memory (RAM) organization assuming
; relocatible code organization (which is an option in project
; properties > mpasm (Global Options)).  Absolute mode generally should
; be used sparingly.
;
; Example of using GPR Uninitialized Data
;
;   GPR_VAR        UDATA
;   MYVAR1         RES        1      ; User variable linker places
;   MYVAR2         RES        1      ; User variable linker places
;   MYVAR3         RES        1      ; User variable linker places
;
;   ; Example of using Access Uninitialized Data Section (when available)
;   ; The variables for the context saving in the device datasheet may need
;   ; memory reserved here.
;   INT_VAR        UDATA_ACS
;   W_TEMP         RES        1      ; w register for context saving (ACCESS)
;   STATUS_TEMP    RES        1      ; status used for context saving
;   BSR_TEMP       RES        1      ; bank select used for ISR context saving
;
;*******************************************************************************

; TODO PLACE VARIABLE DEFINITIONS GO HERE
var	UDATA	    0X400
colorBitCounter	    RES	    1
colorSwitchOn	    RES	    1

;*******************************************************************************
; Reset Vector
;*******************************************************************************
RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    DEBUT                   ; go to beginning of program
    

;*******************************************************************************
; TODO Step #4 - Interrupt Service Routines
;
; There are a few different ways to structure interrupt routines in the 8
; bit device families.  On PIC18's the high priority and low priority
; interrupts are located at 0x0008 and 0x0018, respectively.  On PIC16's and
; lower the interrupt is at 0x0004.  Between device families there is subtle
; variation in the both the hardware supporting the ISR (for restoring
; interrupt context) as well as the software used to restore the context
; (without corrupting the STATUS bits).
;
; General formats are shown below in relocatible format.
;
;------------------------------PIC16's and below--------------------------------
;
; ISR       CODE    0x0004           ; interrupt vector location
;
;     <Search the device datasheet for 'context' and copy interrupt
;     context saving code here.  Older devices need context saving code,
;     but newer devices like the 16F#### don't need context saving code.>
;
;     RETFIE
;
;----------------------------------PIC18's--------------------------------------
;
; ISRHV     CODE    0x0008
;     GOTO    HIGH_ISR
; ISRLV     CODE    0x0018
;     GOTO    LOW_ISR
;
; ISRH      CODE                     ; let linker place high ISR routine
; HIGH_ISR
;     <Insert High Priority ISR Here - no SW context saving>
;     RETFIE  FAST
;
; ISRL      CODE                     ; let linker place low ISR routine
; LOW_ISR
;       <Search the device datasheet for 'context' and copy interrupt
;       context saving code here>
;     RETFIE
;
;*******************************************************************************

; TODO INSERT ISR HERE

;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************

MAIN_PROG CODE                      ; let linker place main program

 
;*******************************************************************************
; RGB LEDs Functions
;*******************************************************************************
ColorBitOn:
    
    MOVLB   LATB
    BSF	    LATB,5
    ; attendre 0.82us
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    BCF	    LATB,5
    ; attendre 0.43us
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    Return


ColorBitOff:
    
    MOVLB   LATB
    BSF LATB,5 
    ; attendre 0.32us
    NOP
    NOP
    NOP
    BCF	    LATB,5
    ; attendre 0.93us
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    Return

ColorEnable:
    
    MOVLB	0X04
    MOVLW	0x08
    MOVWF	colorBitCounter			; initialiser la valeur de colorBitCounter à 8
    MOVLW	0X05
    MOVWF	colorSwitchOn			; initialiser la valeur de colorSwitchOn à 5

LoopColorEnable
    CPFSEQ	colorBitCounter,1		; test si le colorSwitchOn = colorBitCounter; skip if =
	    GOTO	SetBitOff
	    GOTO	SetBitOn

    SetBitOff
	    CALL	ColorBitOff		; appel la fonction de mise à 0 du bit
	    GOTO 	EndSetBit

    SetBitOn
	    CALL 	ColorBitOn		; appel la fonction de mise à 1 du bit
    EndSetBit
    DECF	colorBitCounter			; décrémente le colorBitCounter
    MOVLW	0X00
    CPFSEQ	colorBitCounter,1 		; test si le colorBitCounter = 0; skip if = 0
	    GOTO	BackToLoopColorEnable	; appel la routine qui renvoie au début de la boucle
	    RETURN
    BackToLoopColorEnable
	    MOVF 	colorSwitchOn,W,1	; charge colorSwitchOn dans le WREG
	    GOTO 	LoopColorEnable


ColorDisable:
    
    MOVLB	0X04
    MOVLW	0x08
    MOVWF	colorBitCounter			; initialiser la valeur de colorBitCounter ï¿½ 8

loopColorDisable
	MOVLW	0X00
	CPFSEQ	colorBitCounter,1 	; test si le colorBitCounter = 0; skip if = 0
	GOTO	EndIfDisableNull	; appel la routine qui renvoie au début de la boucle
	RETURN
    EndIfDisableNull
	DECF	colorBitCounter
	CALL	ColorBitOff		; appel la fonction de mise à 0 du bit
	GOTO 	loopColorDisable


ColorRed:
    
    CALL ColorDisable
    CALL ColorEnable
    CALL ColorDisable
    RETURN


ColorGreen:

    CALL ColorEnable
    CALL ColorDisable
    CALL ColorDisable
    RETURN


ColorBlue:

    CALL ColorDisable
    CALL ColorDisable
    CALL ColorEnable
    RETURN
    
ColorYellow:
    
    CALL ColorEnable
    CALL ColorEnable
    CALL ColorDisable
    RETURN

ColorWhite:
    
    CALL ColorEnable
    CALL ColorEnable
    CALL ColorEnable
    RETURN
    
ColorOff:

    CALL ColorDisable
    CALL ColorDisable
    CALL ColorDisable
    RETURN
    
LED0_On:
    CALL ColorRed
    CALL ColorDisable
    CALL ColorDisable
    CALL ColorDisable
    RETURN
    
LED1_On:
    CALL ColorDisable
    CALL ColorBlue
    CALL ColorDisable
    CALL ColorDisable
    RETURN
    
LED2_On:
    CALL ColorDisable
    CALL ColorDisable
    CALL ColorGreen
    CALL ColorDisable
    RETURN
    
LED3_On:
    CALL ColorDisable
    CALL ColorDisable
    CALL ColorDisable
    CALL ColorYellow
    RETURN
    
LEDAll_Green:
    CALL ColorGreen
    CALL ColorGreen
    CALL ColorGreen
    CALL ColorGreen
    RETURN

LEDAll_Off:
    CALL ColorDisable
    CALL ColorDisable
    CALL ColorDisable
    CALL ColorDisable
    RETURN
    
 
;*******************************************************************************

    
;*******************************************************************************
; Tempo
;*******************************************************************************
    
Tempo_1s:
    
    
    
    
DEBUT
 
;*******************************************************************************
;			    TESTS SUR LES LEDS VERTES
;*******************************************************************************
 
    ; Configuration initiale LEDs (verte)
;    BANKSEL TRISC       ; Sï¿½lection de la banque pour TRISC
;    CLRF TRISC          ; Configure PORTC comme sortie

    ; Allumer et ï¿½teindre les LEDs
;    loop:
;        CALL TurnOnLD0
;        CALL TunOffLD0
;	 CALL TurnOnLD1
;        CALL TunOffLD1
;	 CALL TurnOnLD2
;        CALL TunOffLD2
;	 CALL TurnOnLD3
;        CALL TunOffLD3
;        GOTO loop

    ; Sous-routine pour allumer toutes les LEDs
;    TurnOnAllLEDs:
;        BANKSEL LATC    ; Sï¿½lection de la banque pour LATC
;        BSF LATC, 4     ; Allume la LED sur RC4
;        BSF LATC, 5     ; Allume la LED sur RC5
;        BSF LATC, 6     ; Allume la LED sur RC6
;        BSF LATC, 7     ; Allume la LED sur RC7
;        RETURN

    ; Sous-routien pour alumer un led par une LED
    ;LD0 On
;    TurnOnLD0:
;	BANKSEL LATC    ; Sï¿½lection de la banque pour LATC
;       BSF LATC, 4     ; Allume la LED sur RC4
;	RETURN

    ;LD1 On
;    TurnOnLD1:
;	BANKSEL LATC    ; Sï¿½lection de la banque pour LATC
;       BSF LATC, 5     ; Allume la LED sur RC4
;	RETURN

    ;LD2 On
;    TurnOnLD2:
;	BANKSEL LATC    ; Sï¿½lection de la banque pour LATC
;       BSF LATC, 6     ; Allume la LED sur RC4
;	RETURN

    ;LD3 On
;    TurnOnLD3:
;	BANKSEL LATC    ; Sï¿½lection de la banque pour LATC
;       BSF LATC, 7     ; Allume la LED sur RC4
;	RETURN
	
	
    ; Sous-routine pour ï¿½teindre toutes les LEDs
;    TurnOffAllLEDs:
;        BANKSEL LATC    ; Sï¿½lection de la banque pour LATC
;        BCF LATC, 4     ; ï¿½teint la LED sur RC4
;        BCF LATC, 5     ; ï¿½teint la LED sur RC5
;        BCF LATC, 6     ; ï¿½teint la LED sur RC6
;        BCF LATC, 7     ; ï¿½teint la LED sur RC7
;        RETURN

    ; Sous-rounie pour ï¿½teindre une LED
    ; LD0 Off
;    TunOffLD0:
;	BANKSEL LATC
;	BCF LATC, 4
;	RETURN
;    
;    ; LD1 Off
;    TunOffLD1:
;	BANKSEL LATC
;	BCF LATC, 5
;	RETURN
;
;    ; LD2 Off
;    TunOffLD2:
;	BANKSEL LATC
;	BCF LATC, 6
;	RETURN
;
;    ; LD3 Off
;    TunOffLD3:
;	BANKSEL LATC
;	BCF LATC, 7
;	RETURN
;
;END
;*******************************************************************************
    
    
;*******************************************************************************
;			TESTS SUR LES LEDS RGBW SK6812
;*******************************************************************************
    
    BANKSEL OSCFRQ
    bsf	    OSCFRQ, 3
    BCF	    OSCFRQ, 1
    
    BANKSEL TRISB
    MOVLW   0x0F
    MOVWF   TRISB
    
    BANKSEL LATB
    

    CALL ColorRed
    CALL ColorDisable
    CALL ColorDisable
    CALL ColorDisable
    
    CALL ColorDisable
    CALL ColorBlue
    CALL ColorDisable
    CALL ColorDisable
    
    CALL ColorDisable
    CALL ColorDisable
    CALL ColorGreen
    CALL ColorDisable
    
    goto $
    
    END

;*******************************************************************************
    
    
    

    
    
    
    
	    
	    
    
    