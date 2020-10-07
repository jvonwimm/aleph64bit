      SUBROUTINE AGCOIL
C-----------------------------------------------------
C!   Build coil geometry
C   Author :B. Bloch-Devaux                      19 March 85
C.       modified for data base access           sept. 87
C.  -Called by AGEOME                  from this .HLB
C.  -Calls GSMATE,GSTMED,GSVOLU,GSPOS, from  GEANT3
C.
C. -Stores extra material needed
C. -Stores extra Tracking Media needed
C. -Builds geometry levels below 'COIL' level
C. -Initialises some search optimisation
C.
C-----------------------------------------------------
      EXTERNAL GHSIGM,NAMIND
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER ( NLIMR = 9 , NLIMZ = 6)
      COMMON / AGECOM / AGLIMR(NLIMR),AGLIMZ(NLIMZ),IAGFLD,IAGFLI,IAGFLO
      PARAMETER (LTAB1=5  ,LTAB2=4 ,LMED=4 )
      DIMENSION TAB1(LTAB1,LMED),TAB2(LTAB2,LMED)
C
C ==============================================================
C     Medium are defined as:
C    1- Coil body medium in field (uniform)  material IMCBY (average)
C       -    -      -    out field             -      -        -
C     -     -     -    in non uniform field   -     -        -
C    2- Coil reinforcements volume in field(uniform)  material IMCRG
C        -        -          -    out field     -      -
C    3- Coil rings volume   in field (uniform)aterial IMCMR (AL)
C        -   -     -      out field           -            -
C                           in non uniform field      -      -
C    4- Coil end-plates volume  in non uniform field    material IMCEP
C
C   For each type of medium the two following rows are filled:
C     TMXFD , DMXMS , DEEMX , EPSIL , STMIN
      DATA TAB1 /
     1 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     2 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     3 20.    ,  0.5  , 0.1  , 0.02 , .05,
     4 20.    ,  0.5  , 0.1  , 0.1  , 1. /
C
C        A    , Z    , DENS  , RADL
      DATA TAB2 /
     1 26.98  ,  12.99  , 1.13  , 21.2    ,
     4  12*0.                                            /
C ----------------------------------------------------------------------
C     Inline function for COG1 Bank access
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
      RIN(K)=RTABL(KCO,K,8)
      ROU(K)=RTABL(KCO,K,9)+RIN(K)
      DR(K) =RTABL(KCO,K,9)
      ZMN(K)=RTABL(KCO,K,10)
      ZWI(K)=RTABL(KCO,K,11)
      ZMX(K)=ZMN(K)+ZWI(K)
C
      IMCRG=15
      IMCMR=9
      IMCEP=20
C ===============================================================
      RMAX=AGLIMR(7)
      ZMAX=AGLIMZ(3)
      KCO=IW(NAMIND('COG1'))
      IF (KCO.EQ.0) GO TO 99
C
C  NEED MATERIAL FOR COIL
C
      IAGMAT=IAGMAT+1
      IMCBY=IAGMAT
      ABSL=10000.*TAB2(1,1)/(6.022*TAB2(3,1)*GHSIGM(5.,8,TAB2(1,1)))
      CALL GSMATE(IMCBY,'COIL BODY AVERAGE  $',TAB2(1,1),TAB2(2,1),
     1            TAB2(3,1),TAB2(4,1),ABSL,0,0)
C   First define fake volumes for central (COBY) and end (COEN)
C parts , using media # 2 and # 3 ( air outside and in non uniform field
C
      PTAB(1) = RIN(1)
      PTAB(2) = RMAX
      PTAB(3) = ZMX(2)
      IMED=2
      CALL GSVOLU('COBY','TUBE',IMED,PTAB,3,IVOL)
      PTAB(3) = 0.5*(ZMAX  -ZMX(2) )
      IMED=3
      CALL GSVOLU('COEN','TUBE',IMED,PTAB,3,IVOL)
C
C  Position in COIL
C
      CALL GSPOS('COBY',1,'COIL',0.,0.,0.,0,'ONLY')
      Z=ZMAX -PTAB(3)
      CALL GSPOS('COEN',1,'COIL',0.,0., Z,0,'ONLY')
      CALL GSPOS('COEN',2,'COIL',0.,0.,-Z,2,'ONLY')
C
C   Tracking media are COIL body ( in uniform,non uniform field and
C     out field)
C   COIL rings region  (air) in and out field , kept as IMEDR
C   and IMEDO , COIL end plates (iron) in non uniform field
C   COIL rings (alu) in uniform (IMEAI) ,non uniform (IMEAN)
C   and zero field (IMEAO)
C
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'COIL BODY IN UNI BZ$',IMCBY,0,IAGFLD,ALFIEL,
     1         TAB1(1,1),TAB1(2,1),TAB1(3,1),TAB1(4,1),TAB1(5,1),0,0)
C      Define coil body  in field 'COBI' as tube and place it in 'COBY'
      PTAB(1) = RIN(1)+DR(4)
      PTAB(2) = RIN(2)
      PTAB(3) = ZMX(2)
      CALL GSVOLU('COBI','TUBE',IAGMED,PTAB,3,IVOL)
      CALL GSPOS('COBI',1,'COBY',0.,0.,0.,0,'ONLY')
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'COIL BODY OUT FIELD$',IMCBY,0,0,ALFIEL,
     1         TAB1(1,1),TAB1(2,1),TAB1(3,1),TAB1(4,1),TAB1(5,1),0,0)
C      Define coil body out field 'COBO' as tube and place it in 'COBY'
      PTAB(1) = RIN(2)
      PTAB(2) =  ROU(1)-DR(4)
      PTAB(3) = ZMX(2)
      CALL GSVOLU('COBO','TUBE',IAGMED,PTAB,3,IVOL)
      CALL GSPOS('COBO',1,'COBY',0.,0.,0.,0,'ONLY')
C
C   Define inner and outer parts of coil for reinforcements
C
      IAGMED=IAGMED+1
      IMEDR=IAGMED
      CALL GSTMED(IAGMED,'COIL REINFOR UNI BZ $',IMCRG,0,IAGFLD,
     1  ALFIEL,TAB1(1,2),TAB1(2,2),TAB1(3,2),TAB1(4,2),TAB1(5,2),0,0)
      PTAB(1)=RIN(1)
      PTAB(2)=RIN(1)+DR(4)
      PTAB(3)=ZMX(2)
      CALL GSVOLU('COIN','TUBE',IAGMED,PTAB,3,IVOL)
      CALL GSPOS('COIN',1,'COBY',0.,0.,0.,0,'ONLY')
      IAGMED=IAGMED+1
      IMEDO=IAGMED
      CALL GSTMED(IAGMED,'COIL REINFOR NO FIELD$',IMCRG,0,0,
     1  ALFIEL,TAB1(1,2),TAB1(2,2),TAB1(3,2),TAB1(4,2),TAB1(5,2),0,0)
      PTAB(1)=ROU(1)-DR(4)
      PTAB(2)=ROU(1)
      PTAB(3)=ZMX(2)
      CALL GSVOLU('COUT','TUBE',IAGMED,PTAB,3,IVOL)
      CALL GSPOS('COUT',1,'COBY',0.,0.,0.,0,'ONLY')
C
C   DEFINE TRACKING MEDIUM 'ALU RINGS'
C
      IAGMED=IAGMED+1
      IMEAI=IAGMED
      CALL GSTMED(IAGMED,'COIL RINGS IN UNI BZ$',IMCMR,0,IAGFLD,
     1  ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      IAGMED=IAGMED+1
      IMEAO=IAGMED
      CALL GSTMED(IAGMED,'COIL RINGS NO FIELD$',IMCMR,0,0,
     1  ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      IAGMED=IAGMED+1
      IMEAN=IAGMED
      CALL GSTMED(IAGMED,'COIL RINGS NON UNI BZ$',IMCMR,0,
     1 IAGFLI,ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),
     2   0,0)
C
C CENTRAL RING
C
      PTAB(1)=ROU(1)-DR(4)
      PTAB(2)=ROU(1)
      PTAB(3)=ZWI(4)
      CALL GSVOLU('COCO','TUBE',IMEAO ,PTAB,3,IVOL)
      CALL GSPOS('COCO',1,'COUT',0.,0.,0.,0,'ONLY')
      PTAB(1)=RIN(1)
      PTAB(2)=RIN(1)+DR(4)
      PTAB(3)=ZWI(4)
      CALL GSVOLU('COCI','TUBE',IMEAI ,PTAB,3,IVOL)
      CALL GSPOS('COCI',1,'COIN',0.,0.,0.,0,'ONLY')
C
C EDGE RING
C
      PTAB(1)=ROU(1)-DR(4)
      PTAB(2)=ROU(1)
      PTAB(3)=0.5*ZWI(4)
      Z=0.5*(2.*ZMX(4)-ZWI(4)-ZMAX  -ZMX(2))
      CALL GSVOLU('COEO','TUBE',IMEAN ,PTAB,3,IVOL)
      CALL GSPOS('COEO',1,'COEN',0.,0.,Z,0,'ONLY')
      PTAB(1)=RIN(1)
      PTAB(2)=RIN(1)+DR(4)
      PTAB(3)=0.5*ZWI(4)
      CALL GSVOLU('COEI','TUBE',IMEAN ,PTAB,3,IVOL)
      CALL GSPOS('COEI',1,'COEN',0.,0.,Z,0,'ONLY')
C
C  MIDDLE RINGS FOR REINFORCEMENT
C
      M=1
      PTAB(1)=RIN(1)
      PTAB(2)=RIN(1)+DR(4)
      PTAB(3)=ZWI(4)
      CALL GSVOLU('COMI','TUBE',IMEAI ,PTAB,3,IVOL)
      PTAB(1)=ROU(1)-DR(4)
      PTAB(2)=ROU(1)
      CALL GSVOLU('COMO','TUBE',IMEAO ,PTAB,3,IVOL)
      DO 10 N=1,6
      Z=ZMN(N+4)+PTAB(3)
      CALL GSPOS('COMI',M,'COIN',0.,0.,Z,0,'ONLY')
      CALL GSPOS('COMI',M+1,'COIN',0.,0.,-Z,0,'ONLY')
      CALL GSPOS('COMO',M,'COUT',0.,0.,Z,0,'ONLY')
      CALL GSPOS('COMO',M+1,'COUT',0.,0.,-Z,0,'ONLY')
      M=M+2
 10   CONTINUE
C
C   Middle reinforcements are empty...fill them with air
C      in mag field (medium# IMEDR ) or out field  (# IMEDO)
C
      IMED=IMEDR
      PTAB(1)=RIN(1)+DR(11)
      PTAB(2)=RIN(1)+DR(4)
      PTAB(3)=0.5*ZWI(11)
      CALL GSVOLU('COII','TUBE',IMED,PTAB,3,IVOL)
      PTAB(1)=ROU(1)-DR(4)
      PTAB(2)=ROU(1)-DR(11)
      CALL GSVOLU('COIO','TUBE',IMEDO,PTAB,3,IVOL)
      CALL GSPOS('COII',1,'COMI',0.,0.,0.,0,'ONLY')
      CALL GSPOS('COIO',1,'COMO',0.,0.,0.,0,'ONLY')
C
C END PLATES 9 CM INOX
C
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'COIL ENDPL NON UNI B$',IMCEP,0,
     1     IAGFLI,ALFIEL,
     1         TAB1(1,4),TAB1(2,4),TAB1(3,4),TAB1(4,4),TAB1(5,4),0,0)
      PTAB(1)=RIN(3)
      PTAB(2)=ROU(3)
      PTAB(3)=0.5*ZWI(3)
      CALL GSVOLU('COEP','TUBE',IAGMED,PTAB,3,IVOL)
      ZOR=0.5*(ZMAX+ZMX(2))
      Z=ZMN(3)+0.5*ZWI(3)-ZOR
      CALL GSPOS('COEP',1,'COEN',0.,0.,Z,0,'ONLY')
C
C    Position end of coil body part in COEN
C
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'COIL BODY NON UNI B$',IMCBY,0,IAGFLI,
     1  ALFIEL,TAB1(1,1),TAB1(2,1),TAB1(3,1),TAB1(4,1),TAB1(5,1),0,0)
      PTAB(1)=RIN(1)+DR(4)
      PTAB(2)=ROU(1)-DR(4)
      PTAB(3)=0.5*(ZWI(1)-ZWI(2)-ZWI(3))
      CALL GSVOLU('COBE','TUBE',IAGMED,PTAB,3,IVOL)
      Z=  ZWI(2)+PTAB(3)-ZOR
      CALL GSPOS('COBE',1,'COEN',0.,0.,Z,0,'ONLY')
C
  99  RETURN
      END
