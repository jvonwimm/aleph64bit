      SUBROUTINE AGEOME
C-----------------------------------------------------------------------
C!  Build geometry for ALEPH detector 'a la GEANT3'
C                                        B. BLOCH DEVAUX 18 MARCH 85
C                                        Modified september 87
C                                     Modified November  90 ( new BPIP)
C
C - modified by : F.Ranjard - 911002
C                 change COIL component # to 13 instead of 9
C - modified by : B.Bloch-Devaux - 911015
C                 introduce SCAL related calls and mods for inner
C                 tracking region ( SAMBA and others..)
C
C   called by    ASIRUN                                   from this .HLB
C   calls   AGBEAM,AGVDET,AGITCH,AGTPCH,AGECAL,AGLCAL,AGSMBA
C           AGCOIL,AGQUAD,AGHCAL,AGMUCH,AGCHCK            from this .HLB
C   calls   GGCLOS,GPMATE,GPTMED,GPROTM,GPVOLU
C           GSMATE,GMATE,GSTMED,GSROTM,GSVOLU,GSPOS    from GEANT3
C.
C. -Checks the consistency of the geometry data
C. -Stores the material table for widely used materials
C. -Stores Tracking Media #1,#2 and #3  as ALEPH volume of air
C.  inside field (resp. outside field,non uniform field) for general use
C. -Store Rotation Matrices #1,2,3 as
C.          1 Identity matrix,
C.          2 Rotation around Y axis (180 deg):end-cap A --> end-cap B
C.          3 Rotation around Z axis (180 deg):right half --> left half
C. -Builds the first and second levels of the tree structure according
C.   to the required fiducial volume and the selected detectors.
C. -Calls specific detector geometry routines if required
C. -Initialises some search optimisation
C. -Closes geometry banks and prints out (IPRIJO(19))the stored quantiti
C. -Get cross-section and energy loss tables
C-----------------------------------------------------------------------
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
      PARAMETER ( NLIMR = 9 , NLIMZ = 6)
      COMMON / AGECOM / AGLIMR(NLIMR),AGLIMZ(NLIMZ),IAGFLD,IAGFLI,IAGFLO
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      PARAMETER (LTAB1=5  ,LMED=10)
      DIMENSION TAB1(LTAB1,LMED)
C
      REAL APVC(3),ZPVC(3),WPVC(3)
      REAL AMYL(3),ZMYL(3),WMYL(3)
      REAL ACO(2),ZCO(2),WCO(2)
      INTEGER LIS(2),AGMEDI
C ==================================================================
C     Medium are defined as:
C    1- air inside magnetic field             material: air    IMAIF
C    2- air outside magnetic field            material: air    IMAOF
C    3- central det. region                   material: air    IMCDT
C    4- EC barrel region                      material: air    IMEBL
C    5- EC endcap region                      material: air    IMEEC
C    6- Coil region                           material: air    IMCOI
C    7- quadrupole region                     material: air    IMQUA
C    8- HC barrel region                      material: air    IMHBL
C    9- HC endcap region                      material: air    IMHEC
C   10- Muon region                           material: air    IMMUO
C
C   For each medium the following row is filled:
C     TMXFD , DMXMS , DEEMX , EPSIL , STMIN
      DATA TAB1 /
     1 60.    ,  0.5  , 0.1  , 0.5  , 1. ,
     2  0.    ,  0.5  , 0.1  , 0.1  , 1. ,
     3 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     4 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     5 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     6 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     7  0.    ,  0.5  , 0.1  , 0.1  , 1. ,
     8 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     9 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     X 20.    ,  0.5  , 0.1  , 0.1  , 1.              /
C
C         PVC - density and composition taken from the Handbook of
C         Chemistry and Physics (61st ed.), p. C-741. The density is
C         given there as 1.36-1.40 g/cm**3 .
C
      DATA NPVC,APVC,ZPVC,WPVC,DPVC
     1   / -3, 1.008, 12.011, 35.453,
     2         1.0  ,  6.0  , 17.0  ,
     3         3.0  ,  2.0  ,  1.0  , 1.38 /
C     Mylar is C5H4O2.....glue is considered equivalent to mylar
C    define it by its components
C
      DATA  NMYL , AMYL , ZMYL , WMYL , DMYL
     1     / -3  ,12.011, 1.008,16.00
     2           , 6.0  , 1.0  , 8.0
     3           , 5.   , 4.   , 2.    ,1.39 /
      DATA NCO  ,    ACO    ,  ZCO    , WCO  , DCO
     1/      2  ,   12.011  , 16.00
     2          ,    6.     ,  8.
     3          ,    0.27289, 0.72711 , 0.001977 /
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
C ----------------------------------------------------------------------
C   Define material numbers
      IMAIF=15
      IMAOF=15
      IMCDT=15
      IMEBL=15
      IMEEC=15
      IMCOI=15
      IMQUA=15
      IMHBL=15
      IMHEC=15
      IMMUO=15
C ==================================================================
C
C   Initialise field flags ,update them if necessary
C
      IAGFLO = 0
      IAGFLI = 1
      IAGFLD = 3
      IF ( ALFIEL.EQ.0.) THEN
         IAGFLD = 0
         IAGFLI = 0
      ENDIF
C
C     Check validity and consistency of required geometry
C
      CALL AGCHCK
C
C      Define   GEANT3 standard materials
C
      CALL GMATE
C
C    we need some more material.....geant3 defines up to 16 of them
C
      CALL GSMATE(17,'SILICON$', 28.09 , 14. , 2.33 , 9.36 , 10. ,0,0)
      CALL GSMATE(18,'CARBON FIBER PLASTIC$',12.,6.,1.97,21.62,43.8,0,0)
      CALL GSMIXT(19,'ARGON GAS ',39.95,18.,0.00178,1,1.)
      CALL GSMATE(20,'INOX$',55.15,  25.7,  8.,  1.76,  17.1,  0,  0)
      CALL GSMATE(21,'EPOXY$',6.235,  3.395,  1.5,  34.5,  65.,  0,  0)
      CALL GSMATE(22,'BRASS$',64.235, 29.38, 8.265, 1.544,  14.8,  0, 0)
      CALL GSMIXT(23,'XENON GAS ',131.3,54.,0.00589,1,1.)
      CALL GSMIXT(24,'CO2 GAS   ',ACO,ZCO,DCO,NCO,WCO)
      CALL GSMIXT(25,'PVC$',APVC,ZPVC,DPVC,NPVC,WPVC)
      CALL GSMIXT(26,'MYLAR OR EQUI. GLUE$',AMYL,ZMYL,DMYL,NMYL,WMYL)
      CALL GSMATE(27,'G10 PC BOARD $',15.48 ,7.84 , 1.7, 19.4, 53.,0,0)
      IAGMAT=27
C
C    Get some more material of general use
C
      CALL AGEMAT
C
C     Define 3 tracking media made out of air in the different field
C     configurations: uniform Bz , no field , non uniform field.
C
      CALL GSTMED(1,'AIR INSIDE FIELD$',IMAIF,0,IAGFLD,ALFIEL,
     1         TAB1(1,1),TAB1(2,1),TAB1(3,1),TAB1(4,1),TAB1(5,1),0,0)
C
C    Define as medium # 2  ALEPH air outside coil field
C
      CALL GSTMED(2,'AIR OUTSIDE FIELD$',IMAOF,0,IAGFLO,ALFIEL,
     1         TAB1(1,2),TAB1(2,2),TAB1(3,2),TAB1(4,2),TAB1(5,2),0,0)
C
C   medium # 3 is aleph air in non uniform field
C
      CALL GSTMED(3,'AIR IN NON UNIFORM B$',IMAIF,0,IAGFLI,
     1  ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      IAGMED=3
C
C   DEFINE GENERAL USE ROTATION MATRICES
C
C  Identity matrix for no transformation...
      CALL GSROTM(1,D90,0.,D90,D90,0.,0.)
C   180 DEGREES AROUND Y AXIS
      CALL GSROTM(2,D90,D180,D90,D90,D180,0.)
C  180 DEGREES AROUND Z AXIS
      CALL GSROTM(3,D90,D180,D90,D270,0.,0.)
      IAGROT=3
C
C   Initialize # of sensitive volumes to 0
C
      IAGSLV=0
C
C   Initialize # of volumes stored for impacts at detector entrance
C
      NAGIMP=0
C
C  DEFINE THE OVERALL FIDUCIAL VOLUME
C
      PTAB(1)=0.
      PTAB(2)=ALRMAX
      PTAB(3)=ALZMAX
      CALL GSVOLU('ALEF','TUBE',1,PTAB,3,IVOL)
C
C  DEFINE THE SUB-DETECTORS
C
C   Central DETector up to TPC
C     active region
C
      IF (IGEOJO(1)+IGEOJO(2)+IGEOJO(3).GT.0) THEN
         PTAB(1)=AGLIMR(1)
         PTAB(2)=AGLIMR(5)
         PTAB(3)=AGLIMZ(1)
         IAGMED=IAGMED+1
         CALL GSTMED(IAGMED,'CENTRAL DET.REGION$',IMCDT,0,IAGFLD,ALFIEL,
     1         TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
         CALL GSVOLU('CDET','TUBE',IAGMED,PTAB,3,IVOL)
         Z=0.
         IR=0
         CALL GSPOS('CDET',1,'ALEF',0.,0.,Z,IR,'ONLY')
C   introduce passive parts region at each end of CDET : volume PASV
C   for  91 , PASV and PASW for 92 ( asymetry between the two end parts)
         IF ( IGEOJO(10).GE.2) THEN
            PTAB(1) = AGLIMR(1)
            PTAB(2) = AGLIMR(3)
            PTAB(3) = 0.5*(AGLIMZ(1)-AGLIMZ(6))
            CALL GSVOLU('PASV','TUBE',IAGMED,PTAB,3,IVOL)
            Z= AGLIMZ(1) - PTAB(3)
            CALL GSPOS('PASV',1,'CDET',0.,0.,Z,0,'ONLY')
            IF ( IGEOJO(10).EQ.2) THEN
               CALL GSPOS('PASV',2,'CDET',0.,0.,-Z,2,'ONLY')
            ELSE IF ( IGEOJO(10).GE.3) THEN
               CALL GSVOLU('PASW','TUBE',IAGMED,PTAB,3,IVOL)
               CALL GSPOS('PASW',1,'CDET',0.,0.,-Z,2,'ONLY')
            ENDIF
         ENDIF
C   Beam pipe region
         CALL AGBEAM
         IF (IGEOJO(1).GT.0 ) CALL AGVDET
         IF (IGEOJO(2).GT.0)  CALL AGITCH
         IF (IGEOJO(3).GT.0 ) CALL AGTPCH
      ENDIF
C
C   E/GAMMA BARREL REGION FROM TPC TO COIL REGION
C
      IF (IGEOJO(4).GT.0) THEN
         PTAB(1)=AGLIMR(5)
         PTAB(2)=AGLIMR(6)
         PTAB(3)=AGLIMZ(1)
         IAGMED=IAGMED+1
         CALL GSTMED(IAGMED,'EC BARREL REGION$',IMEBL,0,IAGFLD,ALFIEL,
     1         TAB1(1,4),TAB1(2,4),TAB1(3,4),TAB1(4,4),TAB1(5,4),0,0)
         CALL GSVOLU('ECBL','TUBE',IAGMED,PTAB,3,IVOL)
         IR=0
         CALL GSPOS('ECBL',1,'ALEF',0.,0.,0.,IR,'ONLY')
C
      ENDIF
      IF (IGEOJO(4)+IGEOJO(5)+IGEOJO(6)+IGEOJO(9).GT.0) THEN
C   E/GAMMA END CAPS REGION
         IAGMED=IAGMED+1
         CALL GSTMED(IAGMED,'EC ENDCAP REGION$',IMEEC,0,IAGFLD,ALFIEL,
     1         TAB1(1,5),TAB1(2,5),TAB1(3,5),TAB1(4,5),TAB1(5,5),0,0)
C
         PTAB(1)=0.
         PTAB(2)=D360
         PTAB(3)=2
         PTAB(4)=AGLIMZ(1)
         PTAB(5)=AGLIMR(1)
         PTAB(6)=AGLIMR(6)
         PTAB(7)=AGLIMZ(2)
         PTAB(8)=PTAB(5)
         PTAB(9)=PTAB(6)
         NPAR=9
         IF (IGEOJO(10).GE.2) THEN
            PTAB(3)=4
            PTAB(7)=AGLIMZ(5)
            PTAB(10)=AGLIMZ(5)+0.001
            PTAB(11)=AGLIMR(9)
            PTAB(12)=PTAB(6)
            PTAB(13)=AGLIMZ(2)
            PTAB(14)=AGLIMR(9)
            PTAB(15)=PTAB(6)
            NPAR=15
         ENDIF
         CALL GSVOLU('ECEA','PCON',IAGMED,PTAB,NPAR,IVOL)
         CALL GSVOLU('ECEB','PCON',IAGMED,PTAB,NPAR,IVOL)
         CALL GSPOS('ECEA',1,'ALEF',0.,0.,0.,1,'ONLY')
         CALL GSPOS('ECEB',1,'ALEF',0.,0.,0.,2,'ONLY')
         IF (IGEOJO(4) .GT.0 ) CALL AGECAL
         IF (IGEOJO(5) .GT.0 ) CALL AGLCAL
         IF (IGEOJO(6) .GT.0 ) CALL AGSMBA
         IF (IGEOJO(9) .GT.0 ) CALL AGSCAL
      ENDIF
C
C   COIL VOLUME UP TO HADRON BARREL
C
      IF (IGEOJO(13).GT.0) THEN
         PTAB(1)=AGLIMR(6)
         PTAB(2)=AGLIMR(7)
         PTAB(3)=AGLIMZ(3)
         IAGMED=IAGMED+1
         CALL GSTMED(IAGMED,'COIL REGION$',IMCOI,0,IAGFLI, ALFIEL,
     1         TAB1(1,6),TAB1(2,6),TAB1(3,6),TAB1(4,6),TAB1(5,6),0,0)
         CALL GSVOLU('COIL','TUBE',IAGMED,PTAB,3,IVOL)
         CALL GSPOS('COIL',1,'ALEF',0.,0.,0.,0,'ONLY')
         CALL AGCOIL
C
      ENDIF
C
C          Define the forward/backward beam regions
C
C   Quadrupole region is air without magnetic field
C
      IF (IGEOJO(11).GT.0) THEN
         PTAB(1)=0.
         PTAB(2)=D360
         PTAB(3)=2
         PTAB(4)=AGLIMZ(2)
         PTAB(5)=0.
         PTAB(6)=AGLIMR(4)
         PTAB(7)=ALZMAX
         PTAB(8)=PTAB(5)
         PTAB(9)=PTAB(6)
         NPAR=9
         IMED= 2
         CALL GSVOLU('QUEA','PCON',IMED,PTAB,NPAR,IVOL)
         CALL GSPOS('QUEA',1,'ALEF',0.,0.,0.,0,'ONLY')
C
C     Set the other side using rotation # 2
C
         CALL GSVOLU('QUEB','PCON',IMED,PTAB,NPAR,IVOL)
         CALL GSPOS('QUEB',1,'ALEF',0.,0.,0.,2,'ONLY')
         CALL AGQUAD
      ENDIF
C
C               Overall HCAL barrel
C
C  Define barrel 'HCBL' as TUBE and place in ALEF
C
      IF (IGEOJO(7).GT.0) THEN
         PTAB(1)=AGLIMR(7)
         PTAB(2)=AGLIMR(8)
         PTAB(3)=AGLIMZ(3)
         IAGMED=IAGMED+1
         CALL GSTMED(IAGMED,'HADRON BL  REGION$',IMHBL,0,0,ALFIEL,
     1         TAB1(1,8),TAB1(2,8),TAB1(3,8),TAB1(4,8),TAB1(5,8),0,0)
         CALL GSVOLU('HCBL','TUBE',IAGMED,PTAB,3,IVOL)
         CALL GSPOS ('HCBL',1,'ALEF',0.,0.,0., 0, 'ONLY')
C
C    Endcaps regions filled with air in non uniformn field.
C
         PTAB(1) = 0.
         PTAB(2) = D360
         PTAB(3) = 4
         PTAB(4)=AGLIMZ(2)
         PTAB(5)=AGLIMR(4)
         PTAB(6)=AGLIMR(6)
         PTAB(7)=AGLIMZ(3)-0.001
         PTAB(8) = PTAB(5)
         PTAB(9)= PTAB(6)
         PTAB(10)=AGLIMZ(3)
         PTAB(11)=AGLIMR(4)
         PTAB(12)=AGLIMR(8)
         PTAB(13)=AGLIMZ(4)
         PTAB(14)=PTAB(11)
         PTAB(15)=PTAB(12)
         IAGMED=IAGMED+1
         CALL GSTMED(IAGMED,'HADRON EC  REGION$',IMHEC,0,IAGFLI, ALFIEL,
     1         TAB1(1,9),TAB1(2,9),TAB1(3,9),TAB1(4,9),TAB1(5,9),0,0)
         CALL GSVOLU('HCEA','PCON',IAGMED,PTAB,15,IVOL)
         CALL GSVOLU('HCEB','PCON',IAGMED,PTAB,15,IVOL)
C
C  Place 2 copies in 'ALEF'
C
         CALL GSPOS('HCEA',1,'ALEF',0.,0.,0.,1, 'ONLY')
         CALL GSPOS('HCEB',1,'ALEF',0.,0.,0.,2, 'ONLY')
         CALL AGHCAL
      ENDIF
C
C              Overall MUON detector
C
      IF (IGEOJO(8).GT.0  ) THEN
         IAGMED=IAGMED+1
         CALL GSTMED(IAGMED,'MUON       REGION$',IMMUO,0,0,ALFIEL,
     1       TAB1(1,10),TAB1(2,10),TAB1(3,10),TAB1(4,10),TAB1(5,10),0,0)
         PTAB(1)=0.
         PTAB(2)=D360
         PTAB(3)=6
         PTAB(4)=-ALZMAX
         PTAB(5)=AGLIMR(4)
         PTAB(6)=ALRMAX
         PTAB(7)=-AGLIMZ(4)
         PTAB(8)=PTAB(5)
         PTAB(9)=PTAB(6)
         PTAB(10)=PTAB(7)+0.01
         PTAB(11)=AGLIMR(8)
         PTAB(12)=PTAB(6)
         PTAB(13)=-PTAB(10)
         PTAB(14)=PTAB(11)
         PTAB(15)=PTAB(12)
         PTAB(16)=-PTAB(7)
         PTAB(17)=PTAB(8)
         PTAB(18)=PTAB(9)
         PTAB(19)=-PTAB(4)
         PTAB(20)=PTAB(5)
         PTAB(21)=PTAB(6)
         CALL GSVOLU('MUON','PCON',IAGMED,PTAB,21,IVOL)
         CALL GSPOS('MUON',1,'ALEF',0.,0.,0.,0,'ONLY')
         CALL AGMUCH
      ENDIF
C
C- END ================================================
C
C - set drawing attributes
      CALL AGSATT
C - close geometry
      CALL GGCLOS
C - fill VOLU banks with sensitive volumes
      CALL BDROP (IW,'IMPA')
      CALL ALBOS ('VOLU',0,LMHLEN+IAGSLV,JVOLU,IGARB)
      IW(JVOLU+LMHCOL) = 1
      IW(JVOLU+LMHROW) = IAGSLV
      KVOLU = JVOLU+LMHLEN
      DO 10 I=1,IAGSLV
         IW(KVOLU+I) = IAGSEN(I,1)
 10   CONTINUE
      CALL BLIST (IW,'C+','VOLU')
C
      RETURN
      END
