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

Var	UDATA_ACS
randomNum	    RES	    1
Sequence	    RES	    10
stage		    RES	    1

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
 
ConfigPWM:
; D�but de la configuration

; Associer le module CCP2 avec le timer 2
MOVLW     b'00000100'
MOVWF     CCPTMRS                

; S�lection de la banque d?adresse
MOVLB    0x02

; Associer le pin RC1 avec la fonction de sortie de CCP2
MOVLW     0x06
MOVWF     RC1PPS

; D�sactivation de la sortie PWM pour configuration ; Fixe la p�riode de PWM
BSF       TRISC, 1 
MOVLW     0xFF
MOVWF     T2PR

; Configuration du module CCP2 et format des donn�es
MOVLW     b'10001100'
MOVWF     CCP2CON                 

; Fixe le rapport cyclique du signal
MOVLW     d'01111111'   ; Valeur arbitraire, ajustez en fonction de vos besoins
MOVWF     CCPR2H 
MOVLW     d'00000001'   ; Valeur arbitraire, ajustez en fonction de vos besoins
MOVWF     CCPR2L 

; Configuration de l?horloge du timer 2 = Fosc/4
MOVLW     b'00000001'
MOVWF     T2CLKCON

; Choix des options du timer 2
MOVLW     b'00000100'
MOVWF     T2CON

; Activation de la sortie PWM
BCF       TRISC, 1

; Fin de la configuration

 
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
    MOVWF	colorBitCounter			; initialiser la valeur de colorBitCounter � 8
    MOVLW	0X05
    MOVWF	colorSwitchOn			; initialiser la valeur de colorSwitchOn � 5

LoopColorEnable
    CPFSEQ	colorBitCounter,1		; test si le colorSwitchOn = colorBitCounter; skip if =
	    GOTO	SetBitOff
	    GOTO	SetBitOn

    SetBitOff
	    CALL	ColorBitOff		; appel la fonction de mise � 0 du bit
	    GOTO 	EndSetBit

    SetBitOn
	    CALL 	ColorBitOn		; appel la fonction de mise � 1 du bit
    EndSetBit
    DECF	colorBitCounter			; d�cr�mente le colorBitCounter
    MOVLW	0X00
    CPFSEQ	colorBitCounter,1 		; test si le colorBitCounter = 0; skip if = 0
	    GOTO	BackToLoopColorEnable	; appel la routine qui renvoie au d�but de la boucle
	    RETURN
    BackToLoopColorEnable
	    MOVF 	colorSwitchOn,W,1	; charge colorSwitchOn dans le WREG
	    GOTO 	LoopColorEnable


ColorDisable:
    
    MOVLB	0X04
    MOVLW	0x08
    MOVWF	colorBitCounter			; initialiser la valeur de colorBitCounter � 8

loopColorDisable
	MOVLW	0X00
	CPFSEQ	colorBitCounter,1 	; test si le colorBitCounter = 0; skip if = 0
	GOTO	EndIfDisableNull	; appel la routine qui renvoie au d�but de la boucle
	RETURN
    EndIfDisableNull
	DECF	colorBitCounter
	CALL	ColorBitOff		; appel la fonction de mise � 0 du bit
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
    
ColorPurple:
    
    CALL ColorEnable
    CALL ColorDisable
    CALL ColorEnable
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
    CALL ColorOff
    CALL ColorOff
    CALL ColorOff
    RETURN
    
LED1_On:
    CALL ColorOff
    CALL ColorBlue
    CALL ColorOff
    CALL ColorOff
    RETURN
    
LED2_On:
    CALL ColorOff
    CALL ColorOff
    CALL ColorGreen
    CALL ColorOff
    RETURN
    
LED3_On:
    CALL ColorOff
    CALL ColorOff
    CALL ColorOff
    CALL ColorPurple
    RETURN
    
LEDAll_Green:
    CALL ColorGreen
    CALL ColorGreen
    CALL ColorGreen
    CALL ColorGreen
    RETURN

LEDAll_Off:
    CALL ColorOff
    CALL ColorOff
    CALL ColorOff
    CALL ColorOff
    RETURN
    
LEDAll_Green1:
    CALL ColorGreen
    CALL ColorOff
    CALL ColorGreen
    CALL ColorOff
    RETURN
    
LEDAll_Green2:
    CALL ColorOff
    CALL ColorGreen
    CALL ColorOff
    CALL ColorGreen
    RETURN

LEDAll_Red1:
    CALL ColorRed
    CALL ColorOff
    CALL ColorRed
    CALL ColorOff
    RETURN
    
LEDAll_Red2:
    CALL ColorOff
    CALL ColorRed
    CALL ColorOff
    CALL ColorRed
    RETURN
 
;*******************************************************************************

    
;*******************************************************************************
;		    FONCTION DE TEMPORISATION
;*******************************************************************************

; temporisation 1 seconde (LFINTOSC)
Tempo_1s:
    
    movlw   b'10010000'		    
    movwf   T0CON0, ACCESS	    ; timer1 clock Sosc
    
    movlw   b'10010000'	
    movwf   T0CON1, ACCESS	    ; set les valeurs du registre T0CON1
    
    ; initialiser la valeur du timer � 33536
    movlw   0x86
    movwf   TMR0H		    ; maj TMR0H
    movlw   0xe8		    ; maj TMR0L
    movwf   TMR0L
    
tempo_1s_run    
    btfss   T0CON0,5,ACCESS	    ; tester l'overflow du timer
    goto    tempo_1s_run
    bcf	    T0CON0,7,ACCESS	    ; reset bit de d�marrage
    return

Tempo_0.5s:
    
    movlw   b'10010000'		    
    movwf   T0CON0, ACCESS	    ; timer1 clock Sosc
    
    movlw   b'10010000'	
    movwf   T0CON1, ACCESS	    ; set les valeurs du registre T0CON1
    
    ; initialiser la valeur du timer � 50036
    movlw   0xC3
    movwf   TMR0H		    ; maj TMR0H
    movlw   0x74		    ; maj TMR0L
    movwf   TMR0L
    
tempo_0.5s_run    
    btfss   T0CON0,5,ACCESS	    ; tester l'overflow du timer
    goto    tempo_0.5s_run  
    bcf	    T0CON0,7,ACCESS	    ; reset bit de d�marrage
    return

Tempo_100us:
    
    movlw   b'10010000'		    
    movwf   T0CON0, ACCESS	    ; timer1 clock Sosc
    
    movlw   b'10010000'	
    movwf   T0CON1, ACCESS	    ; set les valeurs du registre T0CON1
    
    ; initialiser la valeur du timer � 65532
    movlw   0xFF
    movwf   TMR0H		    ; maj TMR0H
    movlw   0xFC		    ; maj TMR0L
    movwf   TMR0L
    
tempo_100us_run    
    btfss   T0CON0,5,ACCESS	    ; tester l'overflow du timer
    goto    tempo_100us_run
    bcf	    T0CON0,7,ACCESS	    ; reset bit de d�marrage
    return   
    
;*******************************************************************************

;*******************************************************************************
;			FONCTION RANDOM
;*******************************************************************************


createRandomNum:
    MOVF    T2TMR, W	    ; Chargez la valeur du Timer 2 dans W
    ANDLW   b'00000011'     ; Appliquez un masque pour obtenir les 2 bits de poids faible
    BANKSEL 0x100
    MOVWF   randomNum
    RETURN
    
;*******************************************************************************    
    
;*******************************************************************************
;			FONCTION LED VERTE
;*******************************************************************************

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

;*******************************************************************************   
    
;*******************************************************************************
;			FONCTION BUZZER
;*******************************************************************************    

; Buzzer
BuzzerOnBtn2:
    MOVLW b'01100000' ;
    MOVLB 0x0F
    MOVWF T2PR	    ; fixe la p�riode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN 

BuzzerOnBtn1:
    MOVLW b'01000000' ;
    MOVLB 0x0F
    MOVWF T2PR	    ; fixe la p�riode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN 

BuzzerOnBtn3:
    MOVLW b'01001000' ;
    MOVLB 0x0F
    MOVWF T2PR	    ; fixe la p�riode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN

BuzzerOnBtn0:
    MOVLW b'01010000' ;
    MOVLB 0x0F
    MOVWF T2PR	    ; fixe la p�riode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN

BuzzerOff:
    BSF TRISC, 1	; d�sactivation de la sortie PWM (Buzzer)
    RETURN


    
;*******************************************************************************



;*******************************************************************************
;			FONCTION LEDBUZZ
;*******************************************************************************    

LEDBuzz0:
    Call    LED0_On
    Call    BuzzerOnBtn0
    Call    Tempo0.5s
    Call    BuzzerOff
    RETURN
    
LEDBuzz1:
    Call    LED1_On
    Call    BuzzerOnBtn1
    Call    Tempo0.5s
    Call    BuzzerOff
    RETURN
    
LEDBuzz2:
    Call    LED2_On
    Call    BuzzerOnBtn2
    Call    Tempo0.5s
    Call    BuzzerOff
    RETURN
    
LEDBuzz3:
    Call    LED3_On
    Call    BuzzerOnBtn3
    Call    Tempo0.5s
    Call    BuzzerOff
    RETURN
    
   
    
;*******************************************************************************
    
;*******************************************************************************
;			CONFIG	OSCILLATOR
;*******************************************************************************     
Config_OSC:    
    BANKSEL OSCFRQ
    bsf	    OSCFRQ, 3
    bcf	    OSCFRQ, 1
    
    BANKSEL TRISB
    MOVLW   0x0F
    MOVWF   TRISB
    Return
;*******************************************************************************     
    
;*******************************************************************************
;			    CONFIG BOUTONS
;*******************************************************************************
Config_RGB:
    BANKSEL OSCFRQ
    bsf	    OSCFRQ, 3
    BCF	    OSCFRQ, 1
    
    BANKSEL TRISB
    MOVLW   0x0F
    MOVWF   TRISB
    Return

;*******************************************************************************
    
;*******************************************************************************
;			    CONFIG BOUTONS
;*******************************************************************************
    
; Configuration initiale des Bouttons
Config_Button:
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
    
    Return
    
;*******************************************************************************

;*******************************************************************************
;			    CONFIG DU PWM / BUZZER
;*******************************************************************************

Config_Buzzer:
    ; d?but de la configuration PWM
    MOVLW b'00000100'
    MOVWF CCPTMRS
    ; associe le module CCP2 avec le timer 2
    MOVLB 0x0E
    ; s?lection de la banque d?adresse
    MOVLW 0x06
    MOVWF RC1PPS, 1
    ; associe le pin RC1 avec la fonction de sortie de CCP2
    BSF TRISC, 1    ; d?sactivation de la sortie PWM pour configuration
    MOVLW b'01100000' ;INITIALISE 0
    MOVLB 0x0F
    MOVWF T2PR	    ; fixe la p?riode de PWM (voir formule p.271)
    MOVLW b'10001100'
    MOVWF CCP2CON   ; configuration du module CCP2 et format des donn?es
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
    ; BCF TRISC, 1
    ; activation de la sortie PWM
    ; fin de la configuration
    Return

;*******************************************************************************

DEBUT

    Call    Config_OSC
    Call    Config_RGB
    Call    Config_Button
    Call    Config_Buzzer
    
Game   
    ;Call    CreateRandomNumber
    Call    Menu
    Call    Victory
    Call    Defeat
    ;Call    Sequence
    Goto    Game
    
;*******************************************************************************
;				MENU
;*******************************************************************************

Menu:
    
    Call    LED2_On
    loop_menu
    btfsc   PORTB, 2
    goto    loop_menu
    Call    LEDAll_Off
    Call    Tempo_0.5s
    Return
    
;*******************************************************************************    

;*******************************************************************************
;				SEQUENCE
;*******************************************************************************

;Sequence:
    
    
    Return
    
;******************************************************************************* 
    
;*******************************************************************************
;			SEQUENCE DE VICTOIRE
;*******************************************************************************

Victory:
    
    call    LEDAll_Green1
    call    Tempo_0.5s
    call    LEDAll_Green2
    call    Tempo_0.5s
    call    LEDAll_Green1
    call    Tempo_0.5s
    call    LEDAll_Green2
    call    Tempo_0.5s
    Return
    
;*******************************************************************************
    
;*******************************************************************************
;			SEQUENCE DE DEFAITE
;*******************************************************************************

Defeat:
    
    call    LEDAll_Red1
    call    Tempo_0.5s
    call    LEDAll_Red2
    call    Tempo_0.5s
    call    LEDAll_Red1
    call    Tempo_0.5s
    call    LEDAll_Red2
    call    Tempo_0.5s
    Return
    
;*******************************************************************************
    
;*******************************************************************************
;			TESTS SUR LES LEDS RGBW SK6812
;*******************************************************************************
    CALL    Config_RGB
    CALL    Config_Buzzer
    CALL    Config_Button
    
    MOVLW   10          ; Charge la valeur 9 dans WREG
    MOVWF   stage           ; Stocke la valeur de WREG dans la variable i

    
    MOVLB   0x01		    ; choisir la banque 1
    MOVLW   0x01		    ; Charger l'addresse de la banque de password dans WREG (ici banque = 1)
    MOVWF   FSR0H		    ; Mettre la valeur de WREG dans le registre FSRHigh
    CLRF    FSR0L		    ; reset la valeur de FSRLow � 0 pour s�lectionner l'addresse de password
    ;CALL    test_button_buzzer

loop
    ; Supposons que randomNum est stock� dans un registre sp�cifique
    ; et que les adresses des fonctions sont func1, func2, func3, func4

    
    BANKSEL PORTB        ; S�lectionnez la banque pour PORTB
    BTFSS PORTB, 3       ; Testez si le bouton sur RB3 est press� (1 si enfonc�)
    CALL TestRandom
    
    GOTO loop
    
    BANKSEL PORTB        ; S�lectionnez la banque pour PORTB
    BTFSS PORTB, 3       ; Testez si le bouton sur RB3 est press� (1 si enfonc�)
    CALL BuzzerOnBtn3   ; Appelle la fonction TurnOnAllLEDs si le bouton est press�

TestRandom:
    CALL createRandomNum
    MOVF    randomNum, W    ; Charge la valeur de randomNum dans le registre W
    BTFSC   STATUS, Z       ; Test si la valeur est 0
    GOTO    LED0_Buzzer     ; Appelle func1 si la valeur est 0
    DCFSNZ  WREG, F         ; D�cr�mente W et saute si 0
    GOTO    LED1_Buzzer     ; Appelle func2 si la valeur �tait 1
    DCFSNZ  WREG, F         ; D�cr�mente W et saute si 0
    GOTO    LED2_Buzzer     ; Appelle func3 si la valeur �tait 2
    DCFSNZ  WREG, F         ; D�cr�mente W et saute si 0
    CALL    LED3_Buzzer     ; Appelle func4 si la valeur �tait 3
    main
    RETURN
    
LED0_Buzzer:
    CALL    LED0_On
    CALL    BuzzerOnBtn0
    CALL    Tempo_1s
    CALL    BuzzerOff
    CALL    LEDAll_Off
    GOTO main
    
 
LED1_Buzzer:
    CALL    LED1_On
    CALL    BuzzerOnBtn1
    CALL    Tempo_1s
    CALL    BuzzerOff
    CALL    LEDAll_Off
    GOTO main

LED2_Buzzer:    
    CALL    LED2_On
    CALL    BuzzerOnBtn2
    CALL    Tempo_1s
    CALL    BuzzerOff
    CALL    LEDAll_Off
    GOTO main

LED3_Buzzer:
    CALL    LED3_On
    CALL    BuzzerOnBtn3
    CALL    Tempo_1s
    CALL    BuzzerOff
    CALL    LEDAll_Off
    GOTO main
    
    CALL    LEDAll_Green
    CALL    Tempo_1s
    CALL    BuzzerOff
    CALL    LEDAll_Off
    GOTO main

    
    goto $
	
	
    END

;*******************************************************************************
    
    
    

    
    
    
    
	    
	    
    
    