# Presentation
Le projet Pic18-Simon consiste en la réalisation d'un jeu de Simon à l'aide d'un microcontrôleur de la famille des PIC18, le PIC18F25K40.
Ce jeu consiste à générer aléatoirement une séquence de sons et de couleurs à l’aide d’un buzzer et de 4 LED RGB (Red-Green-Blue) adressables individuellement. 
À l’aide de boutons-poussoir, il s’agit ensuite de reconstituer la séquence dans l’ordre. Si on y arrive, la séquence se complexifie au fur et à mesure, et si on n’y arrive pas, la séquence est remise à zéro.

On dispose du matériel suivant :
- 1 carte PCB
- 1 microcontrôleurs  PIC18F25K40 [datasheet](https://github.com/Nitronix24/Project-PIC18-SIMON/blob/main/Ressources/ds_PIC18F25K40.pdf)
- 4 boutons-poussoirs
- 4 LED SK6812 RGB (CMS) [datasheet](https://github.com/Nitronix24/Project-PIC18-SIMON/blob/main/Ressources/ds_SK6812_LED_serial_RGB.pdf)
- 1 support droit mâle 6 contacts
- 1 buzzer [datasheet](https://github.com/Nitronix24/Project-PIC18-SIMON/blob/main/Ressources/ds_buzzer.pdf)
- 4 condensateurs 100 nF - package 0805 (CMS)
- 5 LED vertes - package 0805 (CMS)
- 10 résistances 2.2 kOhm (CMS)
- 1 condensateur 100 nF non polarisé (céramique)
- 1 résistance 10 kOhm 

# Electronic Assembly
Pour réaliser le montage suivant, vous devez souder les composants ci-dessus selon le schéma suivant :
<center>PCB Scheme<center/>
<img style="margin-left: auto; margin-right: auto; width: 50%; height: 50%" src="https://github.com/Nitronix24/Project-PIC18-SIMON/blob/main/img/PCB_scheme.png"></img>


Pour les composants CMS (Composants Montés en Surface), on peut souder au fer à souder standard, cependant cela étant assez difficile, vous pouvez utiliser un four à refusion pour obtenir un résultat plus propre.

<center><i>PCB CMS Components<i/><center/>

![[PCB_CMS_components_placement.jpg]]
Rouge : résistances 2.2 kOhm
Vert : LED verte - package 0805
Jaune : condensateur 100 nF - package 0805
Bleu : LED SK6812 RGB

**Attention au sens des composants polarisés (LED)**

Pour les composants traversants, vous pouvez les souder au fer à souder standard après avoir soudé les composants CMS.

<center><i>PCB through-hole Components<i/><center/>

![[PCB_through-hole_components_placement.jpg]]
Rouge : bouton
Vert : résistance Master Clear 10 kOhm
Jaune : buzzer
Bleu : PIC18F25K40
Orange : condensateur de découplage 100 nF non polarisé (céramique)
Marron : support droit mâle 6 contacts pour branchement du PicKit3
Noir : fil de sortie de la masse pour les mesures à l'oscilloscope 

Une fois tous les composants ci-dessus soudés, vous devriez obtenir le résultat suivant :
![[PCB_real_components_placement.jpg]]

Les pistes restantes seront expliquées plus tard, car elles serviront pour la mise sur batterie de la carte.

# Assembler programming

## Installation

Pour programmer le PIC18F25K40, vous pouvez utiliser le logiciel propriétaire de microchip MPLAB X dans la version 5.35 qui possède le compilateur assembleur nécessaire.

[Site Microchip](https://www.microchip.com/en-us/tools-resources/archives/mplab-ecosystem)
[MPLAB X IDE](![PCB_through-hole_components_placement](https://github.com/Nitronix24/Project-PIC18-SIMON/assets/96538782/c3d1b24a-dfb9-4d7b-a6ac-a10d720b4faf)
![PCB_scheme](https://github.com/Nitronix24/Project-PIC18-SIMON/assets/96538782/17ff6501-19de-441c-9a02-8488a439dc74)
![PCB_real_components_placement](https://github.com/Nitronix24/Project-PIC18-SIMON/assets/96538782/2ada7310-15e0-4f59-abe1-df6e358c839f)
![PCB_CMS_components_placement](https://github.com/Nitronix24/Project-PIC18-SIMON/assets/96538782/4cace3a5-eaef-4408-81dd-5390e496308c)
https://ww1.microchip.com/downloads/en/DeviceDoc/MPLABX-v5.35-windows-installer.exe)

## Quick Start

Si vous souhaitez uniquement cloner le projet, rendez-vous directement à la partie [[#Clone from repository]].

Une fois le logiciel installé, vous devez créer un projet en suivant les instructions suivantes :

>Categories > Microchip Embedded
Projects > Standalone Project
> Next
Family > All Families
Devices > PIC18F25K40
> Next
Select Tool > PicKit3 ou PicKit 4 (pour faire le lien avec la carte) ou simulator pour lancer le code sans la carte
> Next
Select Compiler > mpasm(v5.87) (path)
> Next
Project Name > [Name]
> Finish

Une fois le projet créé, vous pouvez ajouter un nouveau fichier *pic_8b_general.asm* en faisant :
> clic droit sur le projet > New > pic_8b_general.asm

Vous allez ensuite écrire votre code assembleur dans le fichier ci-dessus.

## Clone from repository
Utilisez le lien suivant [github](https://github.com/Nitronix24/Project-PIC18-SIMON.git) ou téléchargez le ZIP.
Ouvrer le lien avec Github Desktop ou extrayez le ZIP dans votre répertoire de MPLAB X.

Une fois, cela fait, rendez vous dans MPLAB et ouvrer le dossier contenant le projet.

## Run le projet

Pour téléverser votre programme dans le microcontrôleur, vous devez sélectionner votre PicKit et paramétrer l'alimentation du circuit par le PicKit.

> Clic droit sur le projet > Properties > Conf[Default] > Connected Hardware Tool > PicKit(serial number) > Apply

> PicKit > Option categories > Power > Power target circuit from PicKit (YES) > Voltage level > 5.0 > Apply

Une fois, ces deux options sélectionnées, vous pouvez lancer le projet avec *Run Main Project* ou *Debug Main Project*.

## Structure of *Main.asm* file

### Configuration
Lors de la création du fichier *Main.asm*, vous devez ajouter la configuration des bits de votre microcontrôleur.
> Window > Target Memory Views > Configurations Bits

Les paramètres à changer sont les suivants :
> CONFIG1L :
FEXTOSC  =  OFF               ; Pas d’oscillateur externe
RSTOSC    =  n’importe quelle source d’oscillateur interne

> CONFIG3L :
WDTE   =   OFF

> CONFIG4H :
LVP   =  OFF

Les autres bits de configuration peuvent être laissés dans leur état par défaut.

### Variables

Vous devez ensuite créer vos variables sous la forme suivante :

| Nom de l'espace | Code | Numéro de banque |
| --- | --- | :---: |
| var1 | UDATA | 0x100 |
| var2 | UDATA_ACS | |

En dessous de cela on ajoute nos variables :

| Nom de la variable | Réserver | Nombre de case mémoire |
| --- | :---: | :---: |
| integer | RES | 1 |
| string | RES | 6 |

### Main
Au niveau du code suivant, il faut changer le nom de la routine principale, car elle fait conflit avec une bibliothèque du microcontrôleur.
```  
RES_VECT  CODE    0x0000            ; processor reset vector
    Goto    START                   ; go to beginning of program
```
A remplacer par :
```
RES_VECT  CODE    0x0000            ; processor reset vector
	Goto    DEBUT                  ; go to beginning of program
```
**WARNING : ne pas oublier de changer le nom de la routine plus bas dans le code**

### Functions
