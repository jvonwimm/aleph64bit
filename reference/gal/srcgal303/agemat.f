      SUBROUTINE AGEMAT
C -----------------------------------------------------
C! Define materials needed in the ECAL
C   B. Bloch-Devaux    November 1987
C   This routine will disappear when we will use
C  the more general Data Base of material included
C   in the Aleph DB
C --------------------------------------------------------
      REAL AXCO(3),ZXCO(3),WXCO(3)
      REAL ARB(4),ZRB(4),WRB(4)
C! Indices for Ecal related materials
      COMMON/AGECMA/IEMALU,IEMAIR,IEMPVC,IEMBLA,IEMEAV,IEMB12,IEMBS3,
     1        IEMARB,IEMCS0,IEMC12,IEMCS3,IEMLAV,IEMRAV,IEMBAV,IEMCAV
C! Indices for Passive materials related to ECAL environment
      COMMON/AGPMMA/IPMHOF,IPMVEF,IPMTCA,IPMICA,IPMEBX,IPMESP
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      PARAMETER  (D360=TWOPI*RADEG   ,D180=PI*RADEG   ,D90=0.5*D180)
      PARAMETER  (D45=0.5*D90   ,D15=D45/3.   ,D210=7.*D90/3.)
      PARAMETER  (D225=D180+D45   ,D270=D180+D90  ,D7P5=0.5*D15)
      PARAMETER(LSENV=30)
      PARAMETER (LIMVOL=17)
C
      COMMON/AGCONS/ IAGROT,IAGMAT,IAGMED,IAGFHB,IAGSLV,IAGSEN(LSENV,2)
     2      , NAGIMP,LAGIMP(3,LIMVOL)
C
       COMMON /WRKSPC/ WSPACE(88320)
      PARAMETER (LPTAB=50)
      DIMENSION PTAB(LPTAB),JTAB(LPTAB)
      EQUIVALENCE (PTAB(1),JTAB(1),WSPACE(1))
C
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
C
C         Lists for compound-material definitions through GSMIXT.
C
C          Xenon-CO2 mixture (80% and 20%): The percentages are
C          referring to partial pressures. Through the ideal gas
C          equation p*V=const the percentages are in volume when
C          the components have/exert the same pressure. This is
C          then converted to relative weights using the PDG 84
C          values for the densities. The CO2 weight is divided
C          into C and O according to the resp. atomic weights.
C          The overall density is the superposition of the STP
C          (0 deg., 1 atm.) values for Xe and CO2 gases.
C
      DATA NXCO,AXCO,ZXCO,WXCO,DXCO
     1   /  3, 12.011 , 15.999 , 131.3,
     2          6.0   ,  8.0   ,  54.0,
     3          0.0211,  0.0563,   0.9226,  0.0051 /
C
C     Resine filled pressure bag material is defined by components
C
      DATA NRB  ,  ARB  ,  ZRB  ,  WRB  ,  DRB
     1   /  -4  ,1.008  ,12.011 ,14.007 ,15.999,
     2           1.     ,6.     ,7.     ,8.    ,
     3           41.    ,34.    ,1.     ,7.    , 1.2 /
      DATA N1  , N2  , NP  ,ELAY ,EPB1 ,EPB2 ,EPVC
     1   / 10  , 23  ,  2  ,.583 ,.200 ,.400 ,.500 /
C # PLANES STACKS 1,2;#PVC SHEETS;Thicknesses of layer,lead1,lead2,PVC
C ECAL ENDCAP:
      DATA NC1  , NC2  , NCP  ,ECLAY ,ECPB1 ,ECPB2 ,ECALU
     1   / 10  , 23  ,  2  ,.632 ,.200 ,.400 ,.635 /
C # PLANES STACKS 1,2;#ALU SHEETS;Thicknesses of layer,lead1,lead2,ALU
C
C   To compute the stack properties , we use the elementary components:
C    aluminum:#IEMALU          PVC :#IEMPVC
C    Copper  :#IMCOP         Graphite:#IMGRA        Mylar :#IMMYL
C    Gas mix :#IMAXC from Xenon and CO2              G10 PCB :#IMG10
C
      IMGRA=6
      IEMALU=9
      IMCOP=11
      IMLEA=13
      IEMAIR=15
      IMSST=20
      IEMPVC=25
      IMMYL=26
      IMG10=27
      PTAB(1)=39.95
      PTAB(2)=131.3
      PTAB(3)=12.01
      PTAB(4)=16.
      PTAB(11)=18.
      PTAB(12)=54.
      PTAB(13)=6.
      PTAB(14)=8.
      PTAB(21)=1.
      PTAB(22)=1.
      PTAB(23)=0.27289
      PTAB(24)=0.72711
      PTAB(25)=0.9227
      PTAB(26)=0.0211
      PTAB(27)=0.0562
      DIN=0.005107
      IAGMAT=IAGMAT+1
      CALL GSMIXT(IAGMAT,'XE-CO2 GAS 80-20% $',PTAB(2),PTAB(12),DIN,3,
     &     PTAB(25))
C
      IMAXC=IAGMAT
C  Define Tin (Sn)
      IAGMAT=IAGMAT+1
      PTAB(1) = 118.69
      PTAB(11)= 50.
      PTAB(21)= 1.00
      DEN = 7.31
      IMTIN = IAGMAT
      CALL GSMIXT(IAGMAT,'TIN  (Sn) ',PTAB(1),PTAB(11),DEN,1,PTAB(21))
C
C
C   ECAL flat cables are made of copper ( 50.25% ) and pvc (49.75%)
C
      IAGMAT=IAGMAT+1
      IMFCA=IAGMAT
      JTAB(1)=IMCOP
      JTAB(2)=IEMPVC
      PTAB(11)=.5025
      PTAB(12)=.4975
      NAG=2
C   AGMIX  is used here only to prepare A and Z arrays as the weights
C      are already computed
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
      DX=2.4
      CALL GSMIXT(IMFCA,'ECAL FLAT CABLES    $',PTAB(21),PTAB(31),DX,
     1   NAG,   PTAB(11))
      IF (IGEOJO(4).GT.0) THEN
      JTAB(1)=IMMYL
      JTAB(2)=IMCOP
      JTAB(3)=IEMPVC
      JTAB(4)=IMGRA
      JTAB(5)=IEMALU
      JTAB(6)=IMAXC
C
C The thicknesses defined below correspond to the canonical
C pile up of a layer in the barrel ( see ALEPH 86-67 )
C
      PTAB(11)=0.0375
      PTAB(12)=0.0035
      PTAB(13)=0.11
      PTAB(14)=0.0020
      PTAB(15)=0.142
      PTAB(16)=0.288
      NAG=6
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DLAY)
      IAGMAT=IAGMAT+1
      IMLAY=IAGMAT
      CALL GSMIXT(IMLAY,'ECAL BARL WIRE LAYER$',PTAB(21),PTAB(31),DLAY,
     1   NAG,   PTAB(41))
C
      JTAB(1)=IMLAY
      JTAB(2)=IMLEA
      JTAB(3)=IEMPVC
C
C  Average medium for stacks 1 and 2 including the PVC sheets
C
      PTAB(11)=ELAY
      PTAB(12)=EPB1
      NAG=2
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DS12)
C
      IAGMAT=IAGMAT+1
      IEMB12=IAGMAT
      CALL GSMIXT(IEMB12,'EC LIGHT AVERAGE BL$',PTAB(21),PTAB(31),DS12,
     1   NAG,  PTAB(41))
C
      JTAB(1)=IMLAY
      JTAB(2)=IMLEA
C
C  Average medium for stacks 3
      PTAB(11)=ELAY
      PTAB(12)=EPB2
      NAG=2
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DST3)
      IAGMAT=IAGMAT+1
      IEMBS3 =IAGMAT
      CALL GSMIXT(IEMBS3 ,'EC DENSE AVERAGE BL$',PTAB(21),PTAB(31),DST3,
     1   NAG,  PTAB(41))
C
C   ECAL passive material on stack sides is made of
C   flat cables ( 21.88%) , stainless steel 'tirants'(48.51%),
C   pcb Barrettes (29.39%),gas (0.22%)
C
      JTAB(1)=IMFCA
      JTAB(2)=IMSST
      JTAB(3)=IMG10
      JTAB(4)=IMAXC
      PTAB(11)=.2188
      PTAB(12)=.4851
      PTAB(13)=0.2939
      PTAB(14)=0.0022
      NAG=4
      IAGMAT=IAGMAT+1
      IEMBLA=IAGMAT
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
      DX=1.33
      CALL GSMIXT(IEMBLA,'ECAL BL PASSIVE MAT $',PTAB(21),PTAB(31),DX,
     1   NAG,   PTAB(11))
C
C   Define End Part of Barrel
C
      JTAB(11)=IMG10
      JTAB(12)=IMAXC
      PTAB(21)=0.5
      PTAB(22)=0.5
      NAG=2
      CALL AGMIX(NAG,JTAB(11),PTAB(21),PTAB(31),PTAB(41),PTAB(46),DX)
      IAGMAT=IAGMAT+1
      IEMEAV=IAGMAT
      CALL GSMIXT(IAGMAT,'END STACK AVERAGE$',PTAB(31),PTAB(41),
     &   DX,NAG,PTAB(46))
C                            End cap
      IAGMAT=IAGMAT+1
      IEMARB=IAGMAT
      CALL GSMIXT(IEMARB ,'RESIN PRESSURE BAG$',ARB,ZRB,DRB,NRB,WRB)
      JTAB(1)=IMMYL
      JTAB(2)=IMCOP
      JTAB(3)=IMG10
      JTAB(4)=IMGRA
      JTAB(5)=IEMALU
      JTAB(6)=IMAXC
C
C The thicknesses defined below correspond to the canonical
C pile up of a layer in the endcap ( see Mike EDWARDS note)
C
      PTAB(11)=0.040
      PTAB(12)=0.0050
      PTAB(13)=0.155
      PTAB(14)=0.0020
      PTAB(15)=0.142
      PTAB(16)=0.288
      NAG=6
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DLAY)
      IAGMAT=IAGMAT+1
      IMCAY=IAGMAT
      CALL GSMIXT(IMCAY,'ECAL ECAP WIRE LAYER$',PTAB(21),PTAB(31),DLAY,
     1  NAG,     PTAB(41))
C
C  Average medium for stacks 1 and 2 ,standard 'couche'
C
      IAGMAT=IAGMAT+1
C
      JTAB(1)=IMCAY
      JTAB(2)=IMLEA
      PTAB(11)=ECLAY
      PTAB(12)=ECPB1
      NAG=2
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DS12)
      IEMC12=IAGMAT
      CALL GSMIXT(IEMC12,'EC LIGHT AVERAGE EC$',PTAB(21),PTAB(31),DS12,
     1   NAG,  PTAB(41))
C
C  Average medium for stacks 3  in Ecap
      IAGMAT=IAGMAT+1
C
      JTAB(1)=IMCAY
      JTAB(2)=IMLEA
      PTAB(11)=ECLAY
      PTAB(12)=ECPB2
      NAG=2
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DST3)
      IEMCS3 =IAGMAT
      CALL GSMIXT(IEMCS3 ,'EC DENSE AVERAGE EC$',PTAB(21),PTAB(31),DST3,
     1   NAG,  PTAB(41))
      JTAB(1)=IMG10
      JTAB(2)=IMAXC
      JTAB(3)=IMFCA
      JTAB(4)=IEMALU
      JTAB(5)=IEMC12
C
C Try to compute average on left,right,bottom of petal
C  mix PCB , gas , cables (right) +alu and stack (left) +lead (bottom)
C
      PTAB(11)=0.2*.9875+0.765*0.8223
      PTAB(12)=0.2*.0125+0.765*0.0102
      PTAB(13)=0.765*0.1675
      NAG=3
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
      IAGMAT=IAGMAT+1
      IEMRAV=IAGMAT
      CALL GSMIXT(IEMRAV,'ECAP RIGHT AVERAGE $',PTAB(21),PTAB(31),DX,
     1  NAG,     PTAB(41))
      PTAB(11)=1.6*0.7693+0.765*0.8223
      PTAB(12)=1.6*0.0044+0.765*0.0102
      PTAB(13)=0.765*0.1675
      PTAB(14)=1.6*0.2263
      PTAB(15)=0.3
      NAG=5
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
      IAGMAT=IAGMAT+1
      IEMLAV=IAGMAT
      CALL GSMIXT(IEMLAV,'ECAP LEFT AVERAGE  $',PTAB(21),PTAB(31),DX,
     1  NAG,     PTAB(41))
      JTAB(3)=IMLEA
      PTAB(11)=1.6*0.1602+0.165
      PTAB(12)=1.6*0.0006
      PTAB(13)=1.6*0.7921
      PTAB(14)=1.6*0.0471
      PTAB(15)=0.4
      NAG=5
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
      IAGMAT=IAGMAT+1
      IEMBAV=IAGMAT
      CALL GSMIXT(IEMBAV,'ECAP BOTTOM AVERAGE $',PTAB(21),PTAB(31),DX,
     1  NAG,     PTAB(41))
      JTAB(1) = IEMRAV
      JTAB(2) = IEMLAV
      PTAB(11) = 0.965
      PTAB(12) = 2.665
      NAG = 2
      IAGMAT = IAGMAT + 1
      IEMCAV = IAGMAT
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
      CALL GSMIXT(IEMCAV,'EC LEFT+RIGHT AVERAGE$',PTAB(21),PTAB(31),DX,
     1   NAG, PTAB(41))
C     Define stack 0 that is first layer on internal alu plate
        JTAB(1)=IMCAY
        JTAB(2)=IEMALU
        PTAB(11)=ECLAY
        PTAB(12)=ECPB1
        NAG=2
        CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DS0)
        IAGMAT=IAGMAT+1
        IEMCS0=IAGMAT
        CALL GSMIXT(IEMCS0,'EC VERY FIRST LAYER$',PTAB(21),PTAB(31),DS0,
     1   NAG,  PTAB(41))
C     Define support plate material of petals
C    The whole support is made of :
C    Aluminum :28.4 Kg          Stainless Steel : 30.65 Kg
C    Bronze   : 5.  Kg          Fiber glass     :  1.8  Kg
C    an average Bronze composition is 93% copper (Cu), 7 % tin (Sn)
C    Total weight is 65.85 Kg with following %:
C    We take Glass Fiber as equivalent to G10 PC board
C  Al :43.13 %   Fe : 46.55 %   Cu :7.06 %   Sn :0.53 % G10:2.73 %
C
C   AGMIX  is used here only to prepare A and Z arrays as the weights
C      are already computed
      NAG = 5
      JTAB(1) = IEMALU
      JTAB(2) = IMSST
      JTAB(3) = IMCOP
      JTAB(4) = IMTIN
      JTAB(5) = IMG10
      PTAB(11) = 0.4313
      PTAB(12) = 0.4655
      PTAB(13) = 0.0706
      PTAB(14) = 0.0053
      PTAB(15) = 0.0273
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
      DX=2.853
      IAGMAT=IAGMAT+1
      IPMESP = IAGMAT
      CALL GSMIXT(IPMESP,'ECAL PETAL SUPPORT  $',PTAB(21),PTAB(31),DX,
     1   NAG,   PTAB(11))
      ENDIF
      IF (IGEOJO(12).GT.0) THEN
         IAGMAT=IAGMAT+1
         IPMHOF=IAGMAT
         JTAB(1)=IEMALU
         JTAB(2)=IMSST
         PTAB(11)=1.0
         NAG=1
         CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
         DX=0.529
         CALL GSMIXT(IPMHOF,'TPC HORIZONTAL FEET$',PTAB(21),PTAB(31),DX,
     1   NAG,   PTAB(11))
         IAGMAT=IAGMAT+1
         IPMVEF=IAGMAT
         PTAB(11)=0.3028
         PTAB(12)=0.6972
         NAG=2
         CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
         DX=1.64
         CALL GSMIXT(IPMVEF,'TPC VERTICAL  FEET $',PTAB(21),PTAB(31),DX,
     1   NAG,   PTAB(11))
      ENDIF
      IF (IGEOJO(4)*IGEOJO(12)+IGEOJO(7).NE.0) THEN
         IAGMAT=IAGMAT+1
         IPMTCA=IAGMAT
         JTAB(1)=IMCOP
         JTAB(2)=IEMPVC
         PTAB(11)=0.3092
         PTAB(12)=0.6908
         NAG=2
         CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
         DX=1.395
         CALL GSMIXT(IPMTCA,'TPC CABLES AVERAGE$',PTAB(21),PTAB(31),DX,
     1   NAG,   PTAB(11))
         IAGMAT=IAGMAT+1
         IPMICA=IAGMAT
         PTAB(11)=0.4132
         PTAB(12)=0.5868
         NAG=2
         CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
         DX=1.49
         CALL GSMIXT(IPMICA,'ITC CABLES AVERAGE$',PTAB(21),PTAB(31),DX,
     1   NAG,   PTAB(11))
C
         JTAB(1)=IMCOP
         JTAB(2)=IEMALU
         JTAB(3)=IMFCA
         PTAB(11)=.5161
         PTAB(12)=.4193
         PTAB(13)=0.0646
         NAG=3
         IAGMAT=IAGMAT+1
         CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
         IPMEBX=IAGMAT
         DX=1.20
         CALL GSMIXT(IAGMAT,'EC ELEC. BOX AVERAGE$',PTAB(21),PTAB(31),
     1   DX,NAG,   PTAB(11))
      ENDIF
      RETURN
      END
