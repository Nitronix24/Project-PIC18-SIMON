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

;*******************************************************************************
; Reset Vector
;*******************************************************************************
RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    DEBUT                   ; go to beginning of program
    
; PIC18F25K40 Configuration Bit Settings

; Assembly source line config statements

; Assembly source line config statements

#include "p18f25k40.inc"

; CONFIG1L
  CONFIG  FEXTOSC = OFF         ; External Oscillator mode Selection bits (Oscillator not enabled)
  CONFIG  RSTOSC = HFINTOSC_64MHZ ; Power-up default value for COSC bits (Secondary Oscillator)

; CONFIG1H
  CONFIG  CLKOUTEN = On        ; Clock Out Enable bit (CLKOUT function is disabled)
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

DEBUT

    ; TODO Step #5 - Insert Your Program Here
    
    ; Configuration initiale LEDs (verte)
    BANKSEL TRISC       ; S�lection de la banque pour TRISC
    CLRF TRISC          ; Configure PORTC comme sortie

    ; Configuration initiale des Bouttons
    BANKSEL TRISB   ; S�lectionnez la banque pour TRISB
    BSF TRISB, 0    ; Mettre le 0�me bit de TRISB � 1 pour configurer RB3 comme entr�e
    BSF TRISB, 1    ; R�p�ter pour les 3 autres boutons
    BSF TRISB, 2
    BSF TRISB, 3
    BANKSEL ANSELB
    CLRF ANSELB, 0    ; Mettre le 0�me bit de ANSELB � O pour configurer RB3 comme entr�e
    CLRF ANSELB, 1    ; R�p�ter pour les 3 autres boutons
    CLRF ANSELB, 2
    CLRF ANSELB, 3
    
    ; d�but de la configuration PWM
    MOVLW b'00000100'
    MOVWF CCPTMRS
    ; associe le module CCP2 avec le timer 2
    MOVLB 0x0E
    ; s�lection de la banque d?adresse
    MOVLW 0x06
    MOVWF RC1PPS, 1
    ; associe le pin RC1 avec la fonction de sortie de CCP2
    BSF TRISA, 6    ; d�sactivation de la sortie PWM pour configuration
    MOVLW b'01100000' ;INITIALISE 0
    MOVLB 0x0F
    MOVWF T2PR	    ; fixe la p�riode de PWM (voir formule p.271) (0 pour le moment)
    MOVLW b'10001100'
    MOVWF CCP2CON   ; configuration du module CCP2 et format des donn�es
    MOVLW d'00000000'
    MOVWF CCPR2H
    MOVLW d'11111111'
    MOVWF CCPR2L
    ; fixe le rapport cyclique du signal (voir formule p.272)
    MOVLW b'0010001'
    MOVWF T2CLKCON
    ; configuration de l'horloge du timer 2 = Fosc/4
    MOVLW b'11110000' ; prescale 1:16, POSTSCALER 1:1
    MOVWF T2CON
    ; choix des options du timer 2 (voir p.256)
    BCF TRISA, 6
    ; activation de la sortie PWM
    ; fin de la configuration
  
    
    ; Allumer et �teindre les LEDs
    loop:
        BANKSEL PORTB        ; S�lectionnez la banque pour PORTB
        BTFSS PORTB, 3       ; Testez si le bouton sur RB3 est press� (1 si enfonc�)
        CALL TurnOnLD3   ; Appelle la fonction TurnOnAllLEDs si le bouton est press�
	
	BTFSC PORTB, 3
	CALL TurnOffLD3

	BTFSS PORTB, 2      ; Testez si le bouton sur RB3 est press� (1 si enfonc�)
        CALL TurnOnLD2   ; Appelle la fonction TurnOnAllLEDs si le bouton est press�
        
	BTFSC PORTB, 2
	CALL TurnOffLD2
	
	BTFSS PORTB, 1       ; Testez si le bouton sur RB3 est press� (1 si enfonc�)
        CALL TurnOnLD1   ; Appelle la fonction TurnOnAllLEDs si le bouton est press�
        
	BTFSC PORTB, 1
	CALL TurnOffLD1
	
	BTFSS PORTB, 0       ; Testez si le bouton sur RB3 est press� (1 si enfonc�)
        CALL TurnOnLD0   ; Appelle la fonction TurnOnAllLEDs si le bouton est press�
        
	BTFSC PORTB, 0
	CALL TurnOffLD0
	;CALL TurnOffAllLEDs 
	GOTO loop

    ; Sous-routine pour allumer toutes les LEDs
    TurnOnAllLEDs:
        BANKSEL LATC    ; S�lection de la banque pour LATC
        BSF LATC, 4     ; Allume la LED sur RC4
        BSF LATC, 5     ; Allume la LED sur RC5
        BSF LATC, 6     ; Allume la LED sur RC6
        BSF LATC, 7     ; Allume la LED sur RC7
        RETURN

    ; Sous-routien pour alumer un led par une LED
    ;LD0 On
    TurnOnLD0:
	BANKSEL LATC    ; S�lection de la banque pour LATC
        BSF LATC, 4     ; Allume la LED sur RC4
	RETURN

    ;LD1 On
    TurnOnLD1:
	BANKSEL LATC    ; S�lection de la banque pour LATC
        BSF LATC, 5     ; Allume la LED sur RC4
	RETURN

    ;LD2 On
    TurnOnLD2:
	BANKSEL LATC    ; S�lection de la banque pour LATC
        BSF LATC, 6     ; Allume la LED sur RC4
	RETURN

    ;LD3 On
    TurnOnLD3:
	BANKSEL LATC    ; S�lection de la banque pour LATC
        BSF LATC, 7     ; Allume la LED sur RC4
	RETURN
	
	
    ; Sous-routine pour �teindre toutes les LEDs
    TurnOffAllLEDs:
        BANKSEL LATC    ; S�lection de la banque pour LATC
        BCF LATC, 4     ; �teint la LED sur RC4
        BCF LATC, 5     ; �teint la LED sur RC5
        BCF LATC, 6     ; �teint la LED sur RC6
        BCF LATC, 7     ; �teint la LED sur RC7
        RETURN

    ; Sous-rounie pour �teindre une LED
    ; LD0 Off
    TurnOffLD0:
	BANKSEL LATC
	BCF LATC, 4
	RETURN
    
    ; LD1 Off
    TurnOffLD1:
	BANKSEL LATC
	BCF LATC, 5
	RETURN

    ; LD2 Off
    TurnOffLD2:
	BANKSEL LATC
	BCF LATC, 6
	RETURN

    ; LD3 Off
    TurnOffLD3:
	BANKSEL LATC
	BCF LATC, 7
	RETURN

    END

    ;commentaire test