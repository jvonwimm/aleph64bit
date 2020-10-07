      SUBROUTINE AGQUAD
C-----------------------------------------------------------------------
C!  Implement Forward and Backward quadrupoles geometry
C  Author :      B. Bloch-Devaux                     18 March 85
C                Modified for Data base access      Septmeber 87
C
C     called from AGEOME                            from this .HLB
C     calls       GSMATE,GSTMED,GSVOLU,GSPOS           from GEANT3
C.
C. -Stores extra material needed
C. -Stores extra Tracking Media needed
C. -Builds geometry levels below 'QUEA' and 'QUEB' levels
C.
C-----------------------------------------------------------------------
      EXTERNAL GHSIGM
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
      PARAMETER ( NLIMR = 9 , NLIMZ = 6)
      COMMON / AGECOM / AGLIMR(NLIMR),AGLIMZ(NLIMZ),IAGFLD,IAGFLI,IAGFLO
      EXTERNAL NAMIND
      CHARACTER*4 QUR(2),QU(2)
      PARAMETER (LTAB1=5,LTAB2=4, LMED=4 )
      INTEGER AGMEDI
      PARAMETER ( INOF=0)
C    INOF means  field tracking flag in quadrupole region , normally 0
C    IAGFLD means field tracking flag inside quadrupole  , normally 3
C
      DIMENSION TAB1(LTAB1,LMED),TAB2(LTAB2,LMED)
C
      DATA QUR/'QUEA','QUEB'/,QU/'QUAA','QUAB'/
C ===================================================================
C     Medium are defined as:
C    1- squad tube volume                    material: air    IMSRG
C    2- squad tube body                      material: average
C    3- pumps and valves medium             material: average
C    4- vacuum  in pipe  medium             material: vacuum
C
C   For each medium the two following rows are filled:
C     TMXFD , DMXMS , DEEMX , EPSIL , STMIN
      DATA TAB1 /
     1  20.   ,  0.5  , 0.1  , 0.1  , 1. ,
     2  20.   ,  0.5  ,  0.1  , 1.  ,  1.  ,
     3  20.   ,  0.5  ,  0.1  , 1.  ,  1.  ,
     4  20.    ,  0.5  ,  0.1  , 1.  ,  1.
     & /
C
C        A    , Z    , DENS  , RADL
      DATA TAB2 /
     1 4*0.            ,
     2      59.7  ,   27.5   ,  2.37   ,   5.62   ,
     3      55.85 ,   26.    ,  0.84   ,   16.5     ,
     4      4*0.
     & /
C -----------------------------------------------------------------
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
      IMSRG = 15
      IAGMAT=IAGMAT+1
      IMPUM=IAGMAT
      ABSL=10000.*TAB2(1,3)/(6.022*TAB2(3,3)*GHSIGM(5.,8,TAB2(1,3)))
      CALL GSMATE(IMPUM,'PUMPING STATION AVERA$',TAB2(1,3),TAB2(2,3),
     1            TAB2(3,3),TAB2(4,3),ABSL,0,0)
      IMVAC=16
C ======================================================================
      RMAX=AGLIMR(4)
      KQU=IW(NAMIND('QUG1'))
      IF (KQU.EQ.0) GO TO 99
C
C   DEFINE MATERIAL AND TRACKING MEDIA FOR QUADR
C
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'PUMPS AND VALVES MED$',IMPUM,0,INOF ,ALFIEL,
     1         TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      IMEDP=IAGMED
      CALL GSVOLU('QUPU','TUBE',IMEDP,PTAB,0,IVOL)
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'VACUUM IN PIPE MED$',IMVAC,0,INOF ,ALFIEL,
     1         TAB1(1,4),TAB1(2,4),TAB1(3,4),TAB1(4,4),TAB1(5,4),0,0)
      IMEDV=IAGMED
      CALL GSVOLU('QUVA','TUBE',IMEDV,PTAB,0,IVOL)
      IMEDQ = AGMEDI(' ')
      IMEDQ = AGMEDI('BEAM VACUUM  MED')
      CALL GSVOLU('QUVI','TUBE',IMEDQ,PTAB,0,IVOL)
      IAGMAT=IAGMAT+1
C
C   Recompute the absorption length for a 5 gev pi+ in this material
C
      ABSL=10000.*TAB2(1,2)/(6.022*TAB2(3,2)*GHSIGM(5.,8,TAB2(1,2)))
      CALL GSMATE(IAGMAT,'QUADR TUBE MATTER$',TAB2(1,2),TAB2(2,2),
     1            TAB2(3,2),TAB2(4,2),ABSL,0,0)
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'QUADR TUBE BODY $',IAGMAT,0,IAGFLD,ALFIEL
     1      ,  TAB1(1,2),TAB1(2,2),TAB1(3,2),TAB1(4,2),TAB1(5,2),0,0)
C
      DO 10 I=1,2
C
C
C  Define  QUAD geometry
         PTAB(1)=0.
         PTAB(2) = RTABL(KQU,2*I-1,9)
         PTAB(3) = 0.5*(ALZMAX-RTABL(KQU,2*I-1,10))
         CALL GSVOLU(QU(I),'TUBE', IAGMED, PTAB,3, IVOL)
C
C Place 'QUAA' and 'QUAB' in 'QUEA(B)'
         Z=ALZMAX-PTAB(3)
         Y=RTABL(KQU,2*I-1,12)
         CALL GSPOS(QU(I),1,QUR(I),0.,Y,Z,0,'ONLY')
C
         PTAB(2)=RTABL(KQU,2*I-1,8)
         Z=0.
         CALL GSPOSP('QUVI',1,QU(I),0.,-Y, Z,0,'ONLY',PTAB,3)
C
C   Implement pumping station regions
C
         PTAB(1)=0.
         PTAB(2)=RTABL(KQU,2*I,9)
         PTAB(3)=0.5*(RTABL(KQU,2*I-1,10)-AGLIMZ(2))
         Z=AGLIMZ(2)+PTAB(3)
         CALL GSPOSP('QUPU',1,QUR(I),0.,0.,Z,0,'ONLY',PTAB,3)
      IF (I.EQ.1) THEN
        PTAB(2)=RTABL(KQU,2*I,8)
        CALL GSPOSP('QUVA',1,'QUPU',0.,0.,0.,0,'ONLY',PTAB,3)
      ENDIF
  10  CONTINUE
  99  RETURN
      END
