      SUBROUTINE AGSATR
C-----------------------------------------------------------------------
C!    Implement Small Angle Track detector geometry
C     Author :                    H.Burkhardt   October 1986
C                              modified for Data Base use october 87
C                              modified for more passive materials
C                              and better implementation (B.Bloch,09/89)
C     Modified: H. Meinhard July 1990 to handle both sides separately
C     (needed to describe the dowels correctly). SATR alignment is now
C     read from bank LALI.
C     Modified by B.Bloch-Devaux October 15, 1991 to handle SATR/SAMBA
C
C      called by AGEOME                      from this .HLB
C. -Calls GSTMED,GSVOLU,GSPOS,GSPOSP,GSROTM  from  GEANT3
C.         AGDIMP                                 this .HLB
C.
C.-Stores extra Tracking Media needed
C.-Builds geometry levels below 'ECEA' ('ECEB') level for SATR
C-----------------------------------------------------------------------
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
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
      COMMON/SATRCO/NSECSA,NCRASA,NCARSA,NTDCSA,NSIDSA,NLAYSA,NBRASA,
     .      RMINSA,RMAXSA,BRTHSA,ZLUMSA,DEADSA,DGASSA,COSTSA,
     .      NHITSA,NLOSSA,NWIRSA,XHITSA,XLOSSA,XWIRSA,XEVTSA,
     .      NHLASA(9),NHLDSA(9),PHIBSA(9)
C
      PARAMETER(JSATID=1,JSATVR=2,JSATCN=4,JSATSU=5,JSATLA=6,JSATSC=7,
     +          JSATWI=8,JSATIR=9,JSATOR=10,JSATIM=11,JSATOM=12,
     +          JSATWD=13,JSATLD=14,JSATLO=15,JSATGL=16,JSATET=17,
     +          JSATBT=18,JSATOT=19,JSATGD=20,JSATMT=21,JSATDL=29,
     +          JSATDR=32,JSATCR=35,JSATCA=36,JSATTD=37,JSATPR=38,
     +          JSATPP=39,JSATPD=40,JSATPA=41,JSATBN=45,JSATBX=46,
     +          JSATBY=47,JSATBZ=48,JSATBR=49,JSATMP=50,JSATFR=56,
     +          JSATFD=57,LSATRA=57)
      PARAMETER (LTAB1=5,LMED=7)
      PARAMETER ( NLIMR = 9 , NLIMZ = 6)
      COMMON / AGECOM / AGLIMR(NLIMR),AGLIMZ(NLIMZ),IAGFLD,IAGFLI,IAGFLO
      DIMENSION TAB1(LTAB1,LMED)
      DIMENSION PHDOW(6)
      CHARACTER SABR*4
C
      INTEGER  GTSTUP
      EXTERNAL NAMIND,GTSTUP
C============================================================
C     Medium are defined as:
C    1-SATR overall volume                    material: MSAIR
C    2-SATR walls epoxy/glass fiber G10       material: MSG10
C    3-SATR rectangular brass tubes           material: MSBRA
C    4-SATR active volume, argon gas          material: MSARG
C    5-SATR dowels , brass                    material: MSBRA
C    6-SATR dlectroniclboxes,average material material: MSEBX
C    7-SATR spacer rings, aluminum            material: MSALU
C
C   For each medium the two following rows are filled:
C     TMXFD , DMXMS , DEEMX , EPSIL , STMIN
      DATA TAB1 /
     1 20.    ,  0.5  , 0.1  , 0.1  , 0.1   ,
     2 20.    ,  0.5  , 0.1  , 0.01 , 0.01  ,
     3 20.    ,  0.5  , 0.1  , 0.005, 0.005 ,
     4 20.    ,  0.5  , 0.1  , 0.1  , 0.1   ,
     5 20.    ,  0.5  , 0.1  , 0.1  , 0.3   ,
     6 20.    ,  0.5  , 0.1  , 0.1  , 0.1   ,
     7 20.    ,  0.5  , 0.1  , 0.1  , 0.1   /
C number of dowels (if not already in data base)
      DATA NDOW/6/
C phi position of dowels (if not already in data base)
      DATA PHDOW/67.5,112.5,172.5,247.5,292.5,352.5/
C radius of brass core of dowels (if not already in data base)
      DATA RADOW/1.0/
C reference z position
      DATA ZREF/262.5/
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
C----------------------------------------------------------------------
C   init geometry for SATR or SAMBA depending on setup number
      ISAST = GTSTUP('PM',1)
      IF ( ISAST.GE.1 ) THEN
         CALL AGSMBA
         GO TO 999
      ENDIF
C---- access geometry banks stored in the data base
C     initialize pointers to SATR bank
      JSATR=IW(NAMIND('SATR'))
      IF(JSATR.EQ.0) GO TO 999
C
C     initialize pointers to LSLO, LALI banks
C
      KLSLO = IW(NAMIND('LSLO'))
      KLALI = IW(NAMIND('LALI'))
      IF (KLSLO .EQ. 0 .OR. KLALI .EQ. 0)                   GOTO 999
C
C     standard media used are 15 for AIR , 19 for ARGON and 22 for BRASS
C    9 for Aluminum , 11 for copper and 27 for G10.
C
      MSAIR=15
      MSARG=19
      MSBRA=22
      MSG10=27
      MSCOP = 11
      MSALU = 9
C   compute average material for electronic boxes:
C   268.8 cm3 copper, 1667.5 cm3 alumium , 851.8 cm3 G10
C   total volume for one box:14.x32.x14.5 = 6496 cm3
C   so d= sum (vol*density)/ total volume
C        = 8358.8/6496 =1.287g/cm3
C
C   copper = 28.81%, aluminum = 53.86% ., G10 = 17.33% in weight
      IAGMAT = IAGMAT+1
      MSEBX = IAGMAT
      JTAB(1) = MSCOP
      JTAB(2) = MSALU
      JTAB(3) = MSG10
      PTAB(11) = .2881
      PTAB(12) = .5386
      PTAB(13) = .1733
      NAG = 3
      CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),PTAB(41),DX)
      DX = 1.287
      CALL GSMIXT(IAGMAT,'SATR ELEC BOX AVERAGE$',PTAB(21),PTAB(31),
     1   DX,NAG,   PTAB(11))
C======================================================================
C
      ISV=IDETJO(6)
      RMAX=SQRT(RTABL(JSATR,1,JSATBX)**2+0.25*RTABL(JSATR,1,JSATBY)**2)
C
C  define SATR as tube of Air including electronic boxes
C --------------------------
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'SATR VOLUME $',MSAIR,0,IAGFLD,ALFIEL,
     1         TAB1(1,1),TAB1(2,1),TAB1(3,1),TAB1(4,1),TAB1(5,1),0,0)
      IMEDA = IAGMED
C
C     define tube parameters rmin,rmax,thickness
C
      PTAB(1)=RTABL(JSATR,1,JSATIR)
      PTAB(2)=MAX(RTABL(JSATR,1,JSATOR),RMAX)
      PTAB(3)=0.5*FLOAT(ITABL(JSATR,1,JSATLA))*RTABL(JSATR,1,JSATLD)
      CALL GSVOLU('SATA','TUBE',IAGMED,PTAB,3,IVOL)
      CALL GSVOLU('SATB','TUBE',IAGMED,PTAB,3,IVOL)
C
C     position the two mother volumes, coordinate system from Endcaps
C     is origin, with Z pointing once forward and once backward
C     x,y postions (ideally at 0)
      X = 0.
      Y = 0.
C     central Z from start position+half thickness
      ZMOD1 = RTABL(KLSLO,1,7) + RTABL(KLALI,1,6)
      ZMOD2 = RTABL(KLSLO,2,7) + RTABL(KLALI,2,6)
      ZMOD3 = RTABL(KLSLO,3,7) + RTABL(KLALI,3,6)
      ZMOD4 = RTABL(KLSLO,4,7) + RTABL(KLALI,4,6)
      ZMEAN = (ABS(ZMOD1) + ABS(ZMOD2) + ABS(ZMOD3) + ABS(ZMOD4)) / 4.
      Z=ZMEAN-PTAB(3)
      ZLUMSA=Z-PTAB(3)
      CALL GSPOS('SATA',1,'ECEA',X,Y,Z,0,'ONLY')
C we introduce a new coordinate system with opposite x axis
C this coordinate system, shifted into ECEB, has a different z axis
C as compared to the standard ALEPH system.
      IAGROT = IAGROT + 1
      CALL GSROTM(IAGROT,90.,180.,90.,90.,0.,0.)
      CALL GSPOS('SATB',1,'ECEB',X,Y,Z,0,'ONLY')
C------------------------------------------------------------
C  Define two outer cylinders for dowel area SASP, electronic box area
C SABX, an inner cylinder for the detector area SACT,all filled with air
C
      PTAB(1) = RTABL(JSATR,1,JSATIR)
      PTAB(2) = RTABL(JSATR,1,JSATPP)-RTABL(JSATR,1,JSATPR)
      PTAB(3) = 0.5*FLOAT(ITABL(JSATR,1,JSATLA))*RTABL(JSATR,1,JSATLD)
      CALL GSVOLU('SACT','TUBE',IAGMED,PTAB,3,IVOL)
      PTAB(1) = RTABL(JSATR,1,JSATPP)-RTABL(JSATR,1,JSATPR)
      PTAB(2) = MIN(RTABL(JSATR,1,JSATOR),RMAX)
      CALL GSVOLU('SASP','TUBE',IAGMED,PTAB,3,IVOL)
      PTAB(1) = MIN(RTABL(JSATR,1,JSATOR),RMAX)
      PTAB(2) = RMAX
      CALL GSVOLU('SABX','TUBE',IAGMED,PTAB,3,IVOL)
      X = 0.
      Y = 0.
      Z = 0.
      CALL GSPOS ('SACT',1,'SATA',X,Y,Z,0,'ONLY')
      CALL GSPOS ('SASP',1,'SATA',X,Y,Z,0,'ONLY')
      CALL GSPOS ('SABX',1,'SATA',X,Y,Z,0,'ONLY')
      CALL GSPOS ('SACT',1,'SATB',X,Y,Z,IAGROT,'ONLY')
      CALL GSPOS ('SASP',1,'SATB',X,Y,Z,0,'ONLY')
      CALL GSPOS ('SABX',1,'SATB',X,Y,Z,0,'ONLY')
C    refer with z to zmin of previous volume (middle=0-half thickness)
      Z0 =-PTAB(3)
C------------------------------------------------------------
C  Define a cylinder for dowel + ground plate subset in each plane
C  filled with air , SASU ( 9 copies)
      PTAB(1) = RTABL(JSATR,1,JSATPP)-RTABL(JSATR,1,JSATPR)
      PTAB(2) = MIN(RTABL(JSATR,1,JSATOR),RMAX)
      PTAB(3) = 0.5*RTABL(JSATR,1,JSATLD)
      CALL GSVOLU('SASU','TUBE',IAGMED,PTAB,3,IVOL)
      Z = Z0 +PTAB(3)
      DO  5 I=1,ITABL(JSATR,1,JSATLA)
C       place one after the other in the SASP
        CALL GSPOS ('SASU',I,'SASP',X,Y,Z,0,'ONLY')
        Z=Z+2.*PTAB(3)
   5  CONTINUE
C------------------------------------------------------------
C  Define a cylinder for  ground plate  in a plane  filled with G10
C  call SAG1  TUBE of G10
      IAGMED=IAGMED+1
      IAG10=IAGMED
      CALL GSTMED(IAGMED,'SATR GR.PLATES$',MSG10,0,IAGFLD,ALFIEL,
     1        TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      PTAB(1) = RTABL(JSATR,1,JSATPP)-RTABL(JSATR,1,JSATPR)
      PTAB(2) = MIN(RTABL(JSATR,1,JSATOR),RMAX)
      PTAB(3) = 0.5*(RTABL(JSATR,1,JSATGL)+RTABL(JSATR,1,JSATET))
      CALL GSVOLU('SAG1','TUBE',IAGMED,PTAB,3,IVOL)
      Z = 0.5*RTABL(JSATR,1,JSATLD)-PTAB(3)
      CALL GSPOS('SAG1',1,'SASU',X,Y,Z,0,'ONLY')
C------------------------------------------------------------
C  Define a cylinder for brass dowel     in a plane
C  call SADO  TUBE of brass
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'Dowel brass volume$',MSBRA,0,IAGFLD,ALFIEL,
     1        TAB1(1,5),TAB1(2,5),TAB1(3,5),TAB1(4,5),TAB1(5,5),0,0)
      PTAB(1) = 0.
      IF (LCOLS(JSATR) .GE. JSATBR) THEN
        PTAB(2) = RTABL(JSATR,1,JSATBR)
        NDOWL = ITABL(JSATR,1,JSATPD)
      ELSE
        PTAB(2) = RADOW
        NDOWL = NDOW
      ENDIF
      PTAB(3) = 0.5*RTABL(JSATR,1,JSATLD)
      CALL GSVOLU('SADO','TUBE',IAGMED,PTAB,3,IVOL)
      Z = 0.
      DO 6 IDOW = 1, NDOWL
        IF (LCOLS(JSATR) .GE. JSATBR) THEN
          ANG = RTABL(JSATR,1,JSATMP-1+IDOW) * DEGRA
        ELSE
          ANG = PHDOW(IDOW) * DEGRA
        ENDIF
        X = RTABL(JSATR,1,JSATPP) * COS(ANG)
        Y = RTABL(JSATR,1,JSATPP) * SIN(ANG)
        CALL GSPOS('SADO',IDOW,'SASU',X,Y,Z,0,'ONLY')
    6 CONTINUE
C------------------------------------------------------------
C define a cylinder for aluminum spacer rings in a plane
C call SARI tube of aluminum
      IAGMED = IAGMED + 1
      CALL GSTMED(IAGMED,'Spacer alu volume$',MSALU,0,IAGFLD,ALFIEL,
     +  TAB1(1,7),TAB1(2,7),TAB1(3,7),TAB1(4,7),TAB1(5,7),0,0)
      PTAB(1) = PTAB(2)
      PTAB(2) = RTABL(JSATR,1,JSATPR)
      PTAB(3)= 0.5*(RTABL(JSATR,1,JSATLD)-RTABL(JSATR,1,JSATGL)-
     +  RTABL(JSATR,1,JSATET))
      CALL GSVOLU('SARI','TUBE',IAGMED,PTAB,3,IVOL)
      Z =-0.5*RTABL(JSATR,1,JSATLD)+PTAB(3)
      DO 7 IDOW = 1, NDOWL
        IF (LCOLS(JSATR) .GE. JSATBR) THEN
          ANG = RTABL(JSATR,1,JSATMP-1+IDOW) * DEGRA
        ELSE
          ANG = PHDOW(IDOW) * DEGRA
        ENDIF
        X = RTABL(JSATR,1,JSATPP) * COS(ANG)
        Y = RTABL(JSATR,1,JSATPP) * SIN(ANG)
        CALL GSPOS('SARI',IDOW,'SASU',X,Y,Z,0,'ONLY')
    7 CONTINUE
C------------------------------------------------------------
C  Define a box for electronic box simulation
C  call SAEL  BOX  of average material MSEBX
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'SATR electronic box' ,MSEBX,0,IAGFLD,ALFIEL,
     1        TAB1(1,6),TAB1(2,6),TAB1(3,6),TAB1(4,6),TAB1(5,6),0,0)
      PTAB(1) = 0.5*(RTABL(JSATR,1,JSATBX)-RTABL(JSATR,1,JSATBN))
      PTAB(2) = 0.5*RTABL(JSATR,1,JSATBY)
      PTAB(3)= 0.5*RTABL(JSATR,1,JSATBZ)
      CALL GSVOLU('SAEL','BOX ',IAGMED,PTAB,3,IVOL)
      X =0.5*(RTABL(JSATR,1,JSATBN)+RTABL(JSATR,1,JSATBX))
      Y =0.
      Z=-Z0-PTAB(3)
      CALL GSPOS ('SAEL',1,'SABX',X,Y,Z,0,'ONLY')
      CALL GSPOS ('SAEL',2,'SABX',-X,Y,Z,0,'ONLY')
C  define three identical inner cylinders (first for layer 1,2,3 second
C  for layer 4,5,6 and third for layer 7,8,9)
C  surrounding the sens. chamber volume,
C  call SATC type TUBE of AIR
C ---------------------------------------------------
      NCYL=3
C
C     define tube parameters rmin,rmax,thickness
      PTAB(1)=RTABL(JSATR,1,JSATIR)
      PTAB(2) = RTABL(JSATR,1,JSATPP)- RTABL(JSATR,1,JSATPR)
      PTAB(3)=0.5*FLOAT(NCYL)*RTABL(JSATR,1,JSATLD)
      Z=Z0+PTAB(3)
      X =0.
      Y =0.
      CALL GSVOLU('SATC','TUBE',IMEDA ,PTAB,3,IVOL)
      DO 10 I=1,NCYL
C       place one after the other in the SATC
        CALL GSPOS ('SATC',I,'SACT',X,Y,Z,0,'ONLY')
        Z=Z+2.*PTAB(3)
  10  CONTINUE
C
C  define again NCYL   (three) inner identical cylinders rotated to
C  each other by 15 degrees representing the layers type 1,2,3
C  radius like mother volume, thickness one third
C  call SALY type TUBE of AIR
C ---------------------------------------------------
C    refer with z to zmin of previous volume (middle=0-half thickness)
      Z=-PTAB(3)
      PTAB(3)=0.5 * RTABL(JSATR,1,JSATLD)
      Z=Z+PTAB(3)
      CALL GSVOLU('SALY','TUBE',IMEDA ,PTAB,3,IVOL)
      DO 11 I=1,NCYL
C       place one after the other in the SATC rotated by 15 deg around Z
        IAGROT=IAGROT+1
        PHI=PHIBSA(I)*RADEG
        CALL GSROTM(IAGROT,90.,PHI,90.,PHI+90.,0.,0.)
        CALL GSPOS ('SALY',I,'SATC',X,Y,Z,IAGROT,'ONLY')
        Z=Z+2.*PTAB(3)
  11  CONTINUE
C
C  put one epoxy gaschannel/groundplate in each SALY
C  tube radius as from mother module
C---------------------------
C    refer with z to zmax of previous volume (middle=0+half thickness)
      Z=+PTAB(3)
      PTAB(3)=0.5*(RTABL(JSATR,1,JSATGL)+RTABL(JSATR,1,JSATET))
      CALL GSVOLU('SAG2','TUBE',IAG10,PTAB,3,IVOL)
C     store beginning of ground plate in z for later use
      ZREF=Z-2.*PTAB(3)
      Z=ZREF+PTAB(3)
C     put in z at the end of each layer
      CALL GSPOS('SAG2',1,'SALY',X,Y,Z,0,'ONLY')
C
C  define Cylinder surrounding the active part with
C  brass tubes, put one in each SALY, tube radius as before
C  call SACY type TUBE of AIR
C     the thickness is the brass tube thickness+the epoxy left on top
      PTAB(3)=0.5*(RTABL(JSATR,1,JSATET)+BRTHSA)
C     refer with z to beginning of ground plate
      Z=ZREF-PTAB(3)
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'SATR LAYER$',MSAIR,0,IAGFLD,ALFIEL,
     1        TAB1(1,1),TAB1(2,1),TAB1(3,1),TAB1(4,1),TAB1(5,1),0,0)
      CALL GSVOLU('SACY','TUBE',IAGMED,PTAB,3,IVOL)
C     put in z at the end of each layer
      CALL GSPOS('SACY',1,'SALY',X,Y,Z,0,'ONLY')
C
C now definde the trapezoid structure containing the
C brass tubes+the epoxy on top, covering 45 deg in phi
C call SAMO type TRD1 of G10
C--------------------------
C     number of layers
      WALL=RTABL(JSATR,1,JSATOT)
      TA225=TAN(22.5*DEGRA)
      PTAB(1)=(RMINSA-WALL)*TA225
      PTAB(2)=(RMAXSA+WALL)*TA225
C     the height is the brass tube thickness+the epoxy left on top
      PTAB(3)=0.5*(RTABL(JSATR,1,JSATET)+BRTHSA)
      PTAB(4)=0.5*( (RMAXSA+WALL)-(RMINSA-WALL) )
C     centered in SALY
      Z=0.
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'SATR GAS BOX$',MSG10,0,IAGFLD,ALFIEL
     1      ,  TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      CALL GSVOLU('SAMO','TRD1',IAGMED,PTAB,4,IVOL)
C     take radial position from average radial thickness
      R=0.5*(RMINSA+RMAXSA)
C     8 rotations by 45 degrees
      DO 20 I=1,NSECSA
        PHI=22.5+FLOAT(I-1)*45.
        IAGROT=IAGROT+1
        CALL GSROTM(IAGROT,90.,90.+PHI,0.,0.,90.,PHI)
        PHI=PHI*DEGRA
        X=R*COS(PHI)
        Y=R*SIN(PHI)
        CALL GSPOS('SAMO',I,'SACY',X,Y,Z,IAGROT,'ONLY')
   20 CONTINUE
C     now put inside the rectangular brass tubes
C     unfortunatly in the present version of GEANT
C     the 14 BRASS tubes of the same type TRD1 and only
C     different length in one dimension have to be defined
C     with 14 different names and entrances in the tree structure
C     since they will still have to be filled with a substructure
C  call them SB01-SB14  type TRD1 of BRASS
      SABR(1:3)='SB0'
      DX1MI=RMINSA*TA225
      DX2MA=RMAXSA*TA225
      XSTEP=(DX2MA-DX1MI)/14.
      X=0.
      Y=+PTAB(3)
      Z=-PTAB(4)+WALL
C     the tubes have a rectangular cross section of 1 cm * 1 cm
      PTAB(3)=0.5*BRTHSA
      PTAB(4)=0.5*BRTHSA
      Y=Y-PTAB(3)
      Z=Z+PTAB(4)
      IAGMED=IAGMED+1
C     medium is brass
      CALL GSTMED(IAGMED,'SATR RECTAN. BRASS TUBES$',MSBRA,0,IAGFLD,
     1    ALFIEL,TAB1(1,2),TAB1(2,2),TAB1(3,2),TAB1(4,2),TAB1(5,2),0,0)
      DO 30 I=1,14
        WRITE(SABR(3:4),'(I2)') I
        IF(SABR(3:3).EQ.' ') SABR(3:3)='0'
        PTAB(1)=DX1MI+FLOAT(I-1)*XSTEP
        PTAB(2)=DX1MI+FLOAT(I  )*XSTEP
        CALL GSVOLU(SABR,'TRD1',IAGMED,PTAB,4,IVOL)
        CALL GSPOS(SABR,I,'SAMO',X,Y,Z,0,'ONLY')
        Z=Z+2.*PTAB(4)
   30 CONTINUE
C     now put gas (Argon) inside the rectangular brass tubes
C     this is the sensitive volume
C  call SABA type TRD1 of Argon
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'SATR GAS IN TUBES$',MSARG,ISV,IAGFLD,ALFIEL
     1      ,TAB1(1,4),TAB1(2,4),TAB1(3,4),TAB1(4,4),TAB1(5,4),0,0)
      CALL GSVOLU('SABA','TRD1',IAGMED,PTAB,0,IVOL)
C     copy  geometrie from the brass tubes, subtract the .3 mm thickness
      BRTHI=RTABL(JSATR,1,JSATBT)
      DX1MI=(RMINSA-BRTHI)*TA225
      DX2MA=(RMAXSA-BRTHI)*TA225
C     subtract the wall thickness
      PTAB(3)=PTAB(3)-BRTHI
      PTAB(4)=PTAB(4)-BRTHI
      XSTEP=(DX2MA-DX1MI)/14.
C     the Gas is centered in the brass tubes
      Y=0.
      Z=0.
      DO 40 I=1,14
        WRITE(SABR(3:4),'(I2)') I
        IF(SABR(3:3).EQ.' ') SABR(3:3)='0'
        PTAB(1)=DX1MI+FLOAT(I-1)*XSTEP
        PTAB(2)=DX1MI+FLOAT(I  )*XSTEP
        CALL GSPOSP('SABA',I,SABR,X,Y,Z,0,'ONLY',PTAB,4)
   40 CONTINUE
C     Define sensitive volume flag as defined if SET 'SATR' is selected
C
      IF(IAGSLV.GE.LSENV) GOTO 998
      IAGSLV=IAGSLV+1
      IAGSEN(IAGSLV,1)=JHOCHA('SABA')
C     store number (1-8) of SAMO which is level 8
      IAGSEN(IAGSLV,2)=8
C
C     Store volume name level in the geometry tree which defines
C     there entrance in the detector
C
      CALL AGDIMP('SATA',3,'SATR')
      CALL AGDIMP('SATB',3,'SATR')
C
      GOTO 999
C
C-not enough space to save sensitive module
C
 998  CONTINUE
      CALL ALTELL('AGSATR: too many sensitive volumes ',0,'STOP')
      GOTO 999
C
C
C-end
C
 999  CONTINUE
      END
