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
  CONFIG  FEXTOSC = OFF		    ; External Oscillator mode Selection bits (Oscillator not enabled)
  CONFIG  RSTOSC = HFINTOSC_64MHZ   ; Power-up default value for COSC bits (HFINTOSC with HFFRQ = 64 MHz and CDIV = 1:1)

; CONFIG1H
  CONFIG  CLKOUTEN = On		    ; Clock Out Enable bit (CLKOUT function is disabled)
  CONFIG  CSWEN = ON		    ; Clock Switch Enable bit (Writing to NOSC and NDIV is allowed)
  CONFIG  FCMEN = ON		    ; Fail-Safe Clock Monitor Enable bit (Fail-Safe Clock Monitor enabled)

; CONFIG2L
  CONFIG  MCLRE = EXTMCLR	    ; Master Clear Enable bit (If LVP = 0, MCLR pin is MCLR; If LVP = 1, RE3 pin function is MCLR )
  CONFIG  PWRTE = OFF		    ; Power-up Timer Enable bit (Power up timer disabled)
  CONFIG  LPBOREN = OFF		    ; Low-power BOR enable bit (ULPBOR disabled)
  CONFIG  BOREN = SBORDIS	    ; Brown-out Reset Enable bits (Brown-out Reset enabled , SBOREN bit is ignored)

; CONFIG2H
  CONFIG  BORV = VBOR_2P45	    ; Brown Out Reset Voltage selection bits (Brown-out Reset Voltage (VBOR) set to 2.45V)
  CONFIG  ZCD = OFF		    ; ZCD Disable bit (ZCD disabled. ZCD can be enabled by setting the ZCDSEN bit of ZCDCON)
  CONFIG  PPS1WAY = ON		    ; PPSLOCK bit One-Way Set Enable bit (PPSLOCK bit can be cleared and set only once; PPS registers remain locked after one clear/set cycle)
  CONFIG  STVREN = ON		    ; Stack Full/Underflow Reset Enable bit (Stack full/underflow will cause Reset)
  CONFIG  DEBUG = OFF		    ; Debugger Enable bit (Background debugger disabled)
  CONFIG  XINST = OFF		    ; Extended Instruction Set Enable bit (Extended Instruction Set and Indexed Addressing Mode disabled)

; CONFIG3L
  CONFIG  WDTCPS = WDTCPS_31	    ; WDT Period Select bits (Divider ratio 1:65536; software control of WDTPS)
  CONFIG  WDTE = OFF		    ; WDT operating mode (WDT Disabled)

; CONFIG3H
  CONFIG  WDTCWS = WDTCWS_7	    ; WDT Window Select bits (window always open (100%); software control; keyed access not required)
  CONFIG  WDTCCS = SC		    ; WDT input clock selector (Software Control)

; CONFIG4L
  CONFIG  WRT0 = OFF		    ; Write Protection Block 0 (Block 0 (000800-001FFFh) not write-protected)
  CONFIG  WRT1 = OFF		    ; Write Protection Block 1 (Block 1 (002000-003FFFh) not write-protected)
  CONFIG  WRT2 = OFF		    ; Write Protection Block 2 (Block 2 (004000-005FFFh) not write-protected)
  CONFIG  WRT3 = OFF		    ; Write Protection Block 3 (Block 3 (006000-007FFFh) not write-protected)

; CONFIG4H
  CONFIG  WRTC = OFF		    ; Configuration Register Write Protection bit (Configuration registers (300000-30000Bh) not write-protected)
  CONFIG  WRTB = OFF		    ; Boot Block Write Protection bit (Boot Block (000000-0007FFh) not write-protected)
  CONFIG  WRTD = OFF		    ; Data EEPROM Write Protection bit (Data EEPROM not write-protected)
  CONFIG  SCANE = ON		    ; Scanner Enable bit (Scanner module is available for use, SCANMD bit can control the module)
  CONFIG  LVP = OFF		    ; Low Voltage Programming Enable bit (HV on MCLR/VPP must be used for programming)

; CONFIG5L
  CONFIG  CP = OFF		    ; UserNVM Program Memory Code Protection bit (UserNVM code protection disabled)
  CONFIG  CPD = OFF		    ; DataNVM Memory Code Protection bit (DataNVM code protection disabled)

; CONFIG5H

; CONFIG6L
  CONFIG  EBTR0 = OFF		    ; Table Read Protection Block 0 (Block 0 (000800-001FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR1 = OFF		    ; Table Read Protection Block 1 (Block 1 (002000-003FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR2 = OFF		    ; Table Read Protection Block 2 (Block 2 (004000-005FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR3 = OFF		    ; Table Read Protection Block 3 (Block 3 (006000-007FFFh) not protected from table reads executed in other blocks)

; CONFIG6H
  CONFIG  EBTRB = OFF		    ; Boot Block Table Read Protection bit (Boot Block (000000-0007FFh) not protected from table reads executed in other blocks)

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
  
var	UDATA	    0X400
colorBitCounter	    RES	    1
colorSwitchOn	    RES	    1

Var	UDATA_ACS
randomNum	    RES	    1

stage		    RES	    1
lastBtnPressed	    RES	    1
 
isValid		    RES	    1
isEndSeq	    RES	    1
isEndGame	    RES	    1
isVictory	    RES	    1

constZero	    RES	    1
constUn		    RES	    1	    
constDeux	    RES	    1	    
constTrois	    RES	    1	
constFF		    RES	    1	    

;*******************************************************************************
; Reset Vector
;*******************************************************************************
	    
RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    MUSIQUE                   ; go to beginning of program
    
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
 
;Fonction pour mettre a l'etat haut 
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

;Fonction pour mettre a l'etat bas
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
    MOVWF	colorBitCounter			; initialiser la valeur de colorBitCounter a 8
    MOVLW	0X05
    MOVWF	colorSwitchOn			; initialiser la valeur de colorSwitchOn a 5

LoopColorEnable
    CPFSEQ	colorBitCounter,1		; test si le colorSwitchOn = colorBitCounter; skip if =
	    GOTO	SetBitOff
	    GOTO	SetBitOn

    SetBitOff
	    CALL	ColorBitOff		; appel la fonction de mise a 0 du bit
	    GOTO 	EndSetBit

    SetBitOn
	    CALL 	ColorBitOn		; appel la fonction de mise a 1 du bit
    EndSetBit
    DECF	colorBitCounter			; decremente le colorBitCounter
    MOVLW	0X00
    CPFSEQ	colorBitCounter,1 		; test si le colorBitCounter = 0; skip if = 0
	    GOTO	BackToLoopColorEnable	; appel la routine qui renvoie au debut de la boucle
	    RETURN
    BackToLoopColorEnable
	    MOVF 	colorSwitchOn,W,1	; charge colorSwitchOn dans le WREG
	    GOTO 	LoopColorEnable


ColorDisable:
    
    MOVLB	0X04
    MOVLW	0x08
    MOVWF	colorBitCounter			; initialiser la valeur de colorBitCounter a 8

loopColorDisable
	MOVLW	0X00
	CPFSEQ	colorBitCounter,1		; test si le colorBitCounter = 0; skip if = 0
	GOTO	EndIfDisableNull		; appel la routine qui renvoie au debut de la boucle
	RETURN
    EndIfDisableNull
	DECF	colorBitCounter
	CALL	ColorBitOff			; appel la fonction de mise a 0 du bit
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
    
    CALL ColorDisable
    CALL ColorEnable
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

ColorOn:

    CALL ColorEnable
    CALL ColorEnable
    CALL ColorEnable
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
    
LEDAll_Red:
    CALL ColorRed
    CALL ColorRed
    CALL ColorRed
    CALL ColorRed
    RETURN

LEDAll_Off:
    CALL ColorOff
    CALL ColorOff
    CALL ColorOff
    CALL ColorOff
    RETURN

LEDAll_On:
    CALL ColorOn
    CALL ColorOn
    CALL ColorOn
    CALL ColorOn
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
    
    ; initialiser la valeur du timer a 33536
    movlw   0x86
    movwf   TMR0H		    ; maj TMR0H
    movlw   0xe8		    ; maj TMR0L
    movwf   TMR0L
    
tempo_1s_run    
    btfss   T0CON0,5,ACCESS	    ; tester l'overflow du timer
    goto    tempo_1s_run
    bcf	    T0CON0,7,ACCESS	    ; reset bit de demarrage
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
    
    

Tempo_10ms:
    
    movlw   b'10010000'		    
    movwf   T0CON0, ACCESS	    ; timer1 clock Sosc
    
    movlw   b'10010000'	
    movwf   T0CON1, ACCESS	    ; set les valeurs du registre T0CON1
    
    ; initialiser la valeur du timer a 65�226
    movlw   0xFE
    movwf   TMR0H		    ; maj TMR0H
    movlw   0xCA		    ; maj TMR0L
    movwf   TMR0L
    
tempo_10ms_run    
    btfss   T0CON0,5,ACCESS	    ; tester l'overflow du timer
    goto    tempo_10ms_run
    bcf	    T0CON0,7,ACCESS	    ; reset bit de d�marrage
    return

Tempo_20ms:
    
    movlw   b'10010000'		    
    movwf   T0CON0, ACCESS	    ; timer1 clock Sosc
    
    movlw   b'10010000'	
    movwf   T0CON1, ACCESS	    ; set les valeurs du registre T0CON1
    
    ; initialiser la valeur du timer a 64916
    movlw   0xFD
    movwf   TMR0H		    ; maj TMR0H
    movlw   0x94		    ; maj TMR0L
    movwf   TMR0L
    
tempo_20ms_run    
    btfss   T0CON0,5,ACCESS	    ; tester l'overflow du timer
    goto    tempo_20ms_run
    bcf	    T0CON0,7,ACCESS	    ; reset bit de d�marrage
    return    
    
Tempo_0.2s:
    
    movlw   b'10010000'		    
    movwf   T0CON0, ACCESS	    ; timer1 clock Sosc
    
    movlw   b'10010000'	
    movwf   T0CON1, ACCESS	    ; set les valeurs du registre T0CON1
    
    ; initialiser la valeur du timer a 58�036
    movlw   0xE2
    movwf   TMR0H		    ; maj TMR0H
    movlw   0xB4		    ; maj TMR0L
    movwf   TMR0L
    
Tempo_0.2s_run    
    btfss   T0CON0,5,ACCESS	    ; tester l'overflow du timer
    goto    Tempo_0.2s_run
    bcf	    T0CON0,7,ACCESS	    ; reset bit de d�marrage
    return
;*******************************************************************************
 

;*******************************************************************************
;			Fonction Random
;*******************************************************************************

createRandomNum:
    MOVF    T2TMR, W		; Chargez la valeur du Timer 2 dans W
    ANDLW   b'00000011'		; Appliquez un masque pour obtenir les 2 bits de poids faible
    BANKSEL 0x100
    MOVWF   randomNum
    RETURN

    
AddRandom:
    CALL createRandomNum
    MOVLB   0x01		    ; choisir la banque 1
  
    MOVLW   0x01		    ; Charger l'addresse de la banque de password dans WREG (ici banque = 1)
    MOVWF   FSR0H		    ; Mettre la valeur de WREG dans le registre FSRHigh
    
    MOVF    randomNum, W
    MOVWF   POSTINC0		    ; ecrire la valeur de WREG a l'emplacement memoire pointe par FSR
    MOVLW   0xFF
    MOVWF   INDF0

    CLRF    FSR1L		    ; R�ninitialise le pointeur de lecture
    RETURN    

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
    
BuzzerOnBtn0:
    MOVLW d'118'  ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerOnBtn1:
    MOVLW d'105' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN 
    
BuzzerOnBtn2:
    MOVLW d'94' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN 

BuzzerOnBtn3:
    MOVLW d'158' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN

BuzzerSi3:
    MOVLW d'62' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN    
    
BuzzerDo4:
    MOVLW d'59' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN    

BuzzerSol4:
    MOVLW d'79' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN 

BuzzerLa4:
    MOVLW d'70' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
    
BuzzerDo5:
    MOVLW d'62' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerRe5:
    MOVLW d'105' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerMi5:
    MOVLW d'94' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerSol5:
    MOVLW d'158' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerDo6:
    MOVLW d'118' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerOff:
    BSF TRISC, 1	; desactivation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerBsFadiese:
    MOVLW d'369' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerSoldiese:
    MOVLW d'415' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerLoseSol:
    MOVLW d'85' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerLoseRe:
    MOVLW d'83' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerLoseMi:
    MOVLW d'73' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
    
BuzzerLoseDo:
    MOVLW d'63' ;
    MOVLB 0x0F
    MOVWF T2PR		; fixe la periode de PWM (voir formule p.271) (0 pour le moment)
    BCF TRISC, 1	; activation de la sortie PWM (Buzzer)
    RETURN
;*******************************************************************************

;*******************************************************************************
;			FONCTION LEDBUZZ
;*******************************************************************************    

LEDBuzz0:
    Call    LED0_On
    Call    Tempo_100us
    Call    BuzzerOnBtn0
    Call    Tempo_0.5s
    Call    BuzzerOff
    Call    Tempo_100us
    Call    LEDAll_Off
    Call    Tempo_100us
    RETURN
    
LEDBuzz1:
    Call    LED1_On
    Call    Tempo_100us
    Call    BuzzerOnBtn1
    Call    Tempo_0.5s
    Call    BuzzerOff
    Call    Tempo_100us
    Call    LEDAll_Off
    Call    Tempo_100us
    RETURN
    
LEDBuzz2:
    Call    LED2_On
    Call    Tempo_100us
    Call    BuzzerOnBtn2
    Call    Tempo_0.5s
    Call    BuzzerOff
    Call    Tempo_100us
    Call    LEDAll_Off
    Call    Tempo_100us
    RETURN
    
LEDBuzz3:
    Call    LED3_On
    Call    Tempo_100us
    Call    BuzzerOnBtn3
    Call    Tempo_0.5s
    Call    BuzzerOff
    Call    Tempo_100us
    Call    LEDAll_Off
    Call    Tempo_100us
    RETURN
    
LEDBuzzInput0:
    Call    LED0_On
    Call    Tempo_100us
    Call    BuzzerOnBtn0
    Call    Tempo_1s
    Call    BuzzerOff
    Call    Tempo_100us
    Call    LEDAll_Off
    Call    Tempo_100us
    RETURN
    
LEDBuzzInput1:
    Call    LED1_On
    Call    Tempo_100us
    Call    BuzzerOnBtn1
    Call    Tempo_1s
    Call    BuzzerOff
    Call    Tempo_100us
    Call    LEDAll_Off
    Call    Tempo_100us
    RETURN
    
LEDBuzzInput2:
    Call    LED2_On
    Call    Tempo_100us
    Call    BuzzerOnBtn2
    Call    Tempo_1s
    Call    BuzzerOff
    Call    Tempo_100us
    Call    LEDAll_Off
    Call    Tempo_100us
    RETURN
    
LEDBuzzInput3:
    Call    LED3_On
    Call    Tempo_100us
    Call    BuzzerOnBtn3
    Call    Tempo_1s
    Call    BuzzerOff
    Call    Tempo_100us
    Call    LEDAll_Off
    Call    Tempo_100us
    RETURN   
    
;*******************************************************************************
    
;*******************************************************************************
;			FONCTION READ SEQUENCE
;*******************************************************************************
    
    
ReadSequence:
     
Comparaison
    Call    Tempo_0.2s
    MOVLB   0x01
    ;Comparaison avec le tableau
    MOVF    INDF1, W   		    ; Charger l'addresse de la banque dans WREG (ici banque = 1)
    INCF    FSR1L
    CPFSEQ  constFF
    GOTO    Affichage
    CLRF    FSR1L
    RETURN
        
Affichage
    
    ;Comparaison registre avec led
    CPFSEQ  constZero
    GOTO    Comp1
    CALL    LEDBuzz0
    GOTO    Comparaison
    
Comp1
    CPFSEQ  constUn
    GOTO    Comp2
    CALL    LEDBuzz1
    GOTO    Comparaison
    
Comp2
    CPFSEQ  constDeux
    GOTO    Comp3
    CALL    LEDBuzz2
    GOTO    Comparaison
    
Comp3
    CPFSEQ  constTrois
    GOTO    Comparaison
    CALL    LEDBuzz3
    GOTO    Comparaison
    
    
    
    
;*******************************************************************************

    
;*******************************************************************************
;			FONCTION AWAIT RESPONSE
;*******************************************************************************
   
AwaitButton:
    BANKSEL PORTB
    BTFSC   PORTB,  0
	GOTO	Led1
	CALL	LEDBuzz0
	Movlw	0x00
	Movwf	lastBtnPressed
	RETURN
	
    Led1
    BTFSC   PORTB,  1
	GOTO	Led2
	CALL	LEDBuzz1
	Movlw	0x01
	Movwf	lastBtnPressed
	RETURN
	
    Led2	
    BTFSC   PORTB,  2
	GOTO	Led3
	CALL	LEDBuzz2
	Movlw	0x02
	Movwf	lastBtnPressed
	RETURN
	
    Led3
    BTFSC   PORTB,  3
	GOTO	ButtonPress_No
	CALL	LEDBuzz3
	Movlw	0x03
	Movwf	lastBtnPressed
	RETURN
	
    ButtonPress_No
	;CALL	Tempo_100us
	;CALL	LEDAll_Off
	;Call    BuzzerOff
	GOTO	AwaitButton
	
;*******************************************************************************  

;*******************************************************************************  
;			Check Valid
;*******************************************************************************  
CheckValid:
    
    MOVF    INDF1,W          ; Charge la valeur du pointeur de lecture dans WREG pour la comparaison
    CPFSEQ  lastBtnPressed  ; Compare randomNum � 0
    Goto    CheckValid_else
    Movlw   0x01
    Movwf   isValid
    Return
CheckValid_else    
    Movlw   0x00
    Movwf   isValid
    Return
    
;*******************************************************************************  	
	
;*******************************************************************************
;			FUNCTION CHECK END SEQEUNCE
;******************************************************************************* 	

CheckEndSeq:
    INCF    FSR1L
    MOVF    INDF1, W
    CPFSEQ  constFF
    Goto    CheckEndSeq_else
    Movlw   0x01
    Movwf   isEndSeq
    Call    Tempo_0.2s
    call    LEDAll_Green
    Call    BuzzerLoseSol
    call    Tempo_0.5s
    Call    Tempo_0.2s
    call    LEDAll_Off
    Call    BuzzerOff
    Return
CheckEndSeq_else    
    Movlw   0x00
    Movwf   isEndSeq
    Return	
	
;*******************************************************************************
;			FUNCTION CHECK END GAME
;******************************************************************************* 	

CheckEndGame:
    MOVF    FSR1L, W
    CPFSEQ  stage
    Goto    CheckEndGame_else
    Movlw   0x01
    Movwf   isEndGame
    Return
CheckEndGame_else    
    Movlw   0x00
    Movwf   isEndGame
    Return
	
;******************************************************************************* 	
	
;*******************************************************************************
;			FUNCTION MUSIQUE
;*******************************************************************************
    
BabyShark:
    
    ;Re
    call    LED0_On
    Call    BuzzerRe5
    call    Tempo_1s
    ;Mi
    call    LED1_On
    Call    BuzzerMi5
    call    Tempo_1s
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.2s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.2s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
          
    
    ;Re
    call    LED0_On
    Call    BuzzerRe5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Mi
    call    LED1_On
    Call    BuzzerMi5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.2s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.2s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
          
    
    ;Re
    call    LED0_On
    Call    BuzzerRe5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Mi
    call    LED1_On
    Call    BuzzerMi5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.2s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.2s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    
     ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    ;Sol
    call    LED2_On
    Call    BuzzerSol5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_20ms
    
    ;Fa#
    Call    LED3_On
    Call    BuzzerBsFadiese
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_100us
    
    Return
   
MusiquesVST:
    
    ;Mi
    call    LEDAll_On
    Call    BuzzerMi5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Mi
    call    LEDAll_On
    Call    BuzzerMi5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Mi
    call    LEDAll_On
    Call    BuzzerMi5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Do
    call    LEDAll_On
    Call    BuzzerDo4
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Re
    call    LEDAll_On
    Call    BuzzerRe5
    call    Tempo_1s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Re
    call    LEDAll_On
    Call    BuzzerRe5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Re
    call    LEDAll_On
    Call    BuzzerRe5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Re
    call    LEDAll_On
    Call    BuzzerRe5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Si
    call    LEDAll_On
    Call    BuzzerSi3
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Do
    call    LEDAll_On
    Call    BuzzerDo4
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Do
    call    LEDAll_On
    Call    BuzzerDo4
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Re
    call    LEDAll_On
    Call    BuzzerRe5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Mi
    call    LEDAll_On
    Call    BuzzerMi5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Do
    call    LEDAll_On
    Call    BuzzerDo4
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Do
    call    LEDAll_On
    Call    BuzzerDo4
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Do
    call    LEDAll_On
    Call    BuzzerDo4
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;La
    call    LEDAll_On
    Call    BuzzerLa4
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Si
    call    LEDAll_On
    Call    BuzzerSi3
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Re
    call    LEDAll_On
    Call    BuzzerRe5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Do
    call    LEDAll_On
    Call    BuzzerDo4
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Si
    call    LEDAll_On
    Call    BuzzerSi3
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Mi
    call    LEDAll_On
    Call    BuzzerMi5
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Do
    call    LEDAll_On
    Call    BuzzerDo4
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;Si
    call    LEDAll_On
    Call    BuzzerSi3
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;La
    call    LEDAll_On
    Call    BuzzerLa4
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s	
    ;Si
    call    LEDAll_On
    Call    BuzzerSi3
    call    Tempo_1s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;SolDiese
    call    LEDAll_On
    Call    BuzzerSoldiese
    call    Tempo_1s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    ;La
    call    LEDAll_On
    Call    BuzzerLa4
    call    Tempo_0.5s
    Call    BuzzerOff
    Call    LEDAll_Off
    call    Tempo_0.2s
    
    Return
    
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
    BSF	    OSCFRQ, 3
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
    BANKSEL TRISB	; Selectionnez la banque pour TRISB
    BSF TRISB, 0	; Mettre le 0eme bit de TRISB a 1 pour configurer RB3 comme entree
    BSF TRISB, 1	; Repeter pour les 3 autres boutons
    BSF TRISB, 2
    BSF TRISB, 3
    
    BANKSEL ANSELB
    CLRF ANSELB, 0	; Mettre le 0eme bit de ANSELB a O pour configurer RB3 comme entree
    CLRF ANSELB, 1	; Repeter pour les 3 autres boutons
    CLRF ANSELB, 2
    CLRF ANSELB, 3
    
    Return
    
;*******************************************************************************

;*******************************************************************************
;			    CONFIG DU PWM / BUZZER
;*******************************************************************************

Config_Buzzer:
    ; debut de la configuration PWM
    MOVLW b'00000100'
    MOVWF CCPTMRS
    
    ; associe le module CCP2 avec le timer 2
    MOVLB 0x0E
    
    ; selection de la banque d?adresse
    MOVLW 0x06
    MOVWF RC1PPS, 1
    
    ; associe le pin RC1 avec la fonction de sortie de CCP2
    BSF TRISC, 1	    ; desactivation de la sortie PWM pour configuration
    MOVLW b'01100000'	    ;INITIALISE 0
    MOVLB 0x0F	
    MOVWF T2PR		    ; fixe la periode de PWM (voir formule p.271)
    MOVLW b'10001100'
    MOVWF CCP2CON	    ; configuration du module CCP2 et format des donnees
    MOVLW d'00000000'
    MOVWF CCPR2H
    MOVLW d'11111111'
    MOVWF CCPR2L
    
    ; fixe le rapport cyclique du signal (voir formule p.272)
    MOVLW b'0010001'
    MOVWF T2CLKCON
    
    ; configuration de l'horloge du timer 2 = Fosc/4
    MOVLW b'11110000'	    ; prescale 1:16, POSTSCALER 1:1
    MOVWF T2CON
    
    ; choix des options du timer 2 (voir p.256)
    ; BCF TRISC, 1
    ; activation de la sortie PWM
    ; fin de la configuration
    Return

;*******************************************************************************
    
;*******************************************************************************
;			   Config Random
;*******************************************************************************
Config_Random:
    MOVLW   0x10			    ; Charge la valeur 9 dans WREG
    MOVWF   stage		    ; Stocke la valeur de WREG dans la variable stage
    CLRF    FSR0L		    ; reset la valeur de FSRLow � 0 pour s�lectionner l'addresse de s�quence
    
    Movlw   0x00
    Movwf   constZero
    Movlw   0x01
    Movwf   constUn
    Movlw   0x02
    Movwf   constDeux
    Movlw   0x03
    Movwf   constTrois
    Movlw   0xff
    Movwf   constFF
    
    MOVLB   0x01		    ; choisir la banque 1
    MOVLW   0x01		    ; Charger l'addresse de la banque de password dans WREG (ici banque = 1)
    MOVWF   FSR0H		    ; Mettre la valeur de WREG dans le registre FSRHigh
    MOVLW   0x0F
    MOVWF    FSR0L		    ; Met la valeur de FSRLow � 16 pour s�lectionner la derni�re addresse de la sequence
    
    MOVLB   0x01		    ; choisir la banque 1
    MOVLW   0x01		    ; Charger l'addresse de la banque de password dans WREG (ici banque = 1)
    MOVWF   FSR1H		    ; Mettre la valeur de WREG dans le registre FSRHigh
    CLRF    FSR1L		    ; reset la valeur de FSRLow � 0 pour s�lectionner l'addresse de la sequence
    
    sequenceInitTab		    ; Routine qui s'assure que le tableau soit vide avant de commencer a �crire
    CLRF    INDF0
    DECFSZ  FSR0L
    GOTO    sequenceInitTab
    Return
    
;*******************************************************************************
MUSIQUE
    Call    Config_OSC
    Call    Config_RGB
    Call    Config_Button
    Call    Config_Buzzer
    Call    Config_Random
    
    Call    BabyShark
    Goto    DEBUT
    
DEBUT

    Call    Config_OSC
    Call    Config_RGB
    Call    Config_Button
    Call    Config_Buzzer
    Call    Config_Random
;test_loop
   ; Call    BuzzerDo6
    ;Call    Tempo_1s
   ; Call    BuzzerOff
   ; Call    BuzzerDo4
   ; Call    Tempo_1s
    
   ; Call    BuzzerOff
    ;Goto    test_loop
    
Game   
    Call    Menu
Sequence
    Call    AddRandom
    Call    ReadSequence
CheckButton
    Call    AwaitButton
    Call    CheckValid
    Call    CheckEndSeq
    Call    CheckEndGame
    
    Movlw   0x01
    CPFSEQ  isValid
    Goto    Defeat
    Movlw   0x01
    CPFSEQ  isEndSeq
    Goto    CheckButton
    Movlw   0x01
    CPFSEQ  isEndGame
    Goto    Sequence
    Call    Victory
    Goto    DEBUT
    
Defeat
    
    call    LEDAll_Red1
    Call    BuzzerLa4
    call    Tempo_1s
    call    LEDAll_Red2
    Call    BuzzerSol4
    call    Tempo_1s
    call    LEDAll_Red1
    Call    BuzzerLa4
    call    Tempo_1s
    call    LEDAll_Red2
    Call    BuzzerSol4
    call    Tempo_1s
    Call    BuzzerOff
    Goto    DEBUT
    
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
  
    END
    