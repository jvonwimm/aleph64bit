      SUBROUTINE SAIRUN
C-----------------------------------------------------------------------
C!    initialize common variables and Banks for the SATR
C                                H.Burkhardt October 1986
C                        modified for Data Base access October 87
C     Modified H. Meinhard 29-May-1990: Get alignment constants from
C                                       LALI and LSLO banks
C - modified by : B.Bloch-Devaux - 911015
C                 introduce SETUP dependence
C---------------------------------------------------------------------
      SAVE
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
      PARAMETER(LCSAHT=1,MXSAHT=500,LCSADI=1)
      COMMON/SANAMC/NASAHT,NASADI
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
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      INTEGER  ALGTDB,GTSTUP
      EXTERNAL ALGTDB,GTSTUP
      EXTERNAL NAMIND
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
C----------------------------------------
C     initialize variables for the SATR
C     only if SATR requested in the geometry before year 92
      ISAST = GTSTUP('PM',1)
      IF (ISAST.GE.1) GO TO  900
C     get SATR constants from the data base
C
      IND=ALGTDB(LRDBIO,'LALILSLOSATRSLAY',IRUNJO)
      IF(IND.EQ.0) THEN
        CALL ALTELL ('SAIRUN: no geometry banks from DBASE',0,'STOP')
        GOTO 900
      ENDIF
C     link to SATR bank
      KSATR=IW(NAMIND('SATR'))
      IF(KSATR.EQ.0) THEN
        WRITE(LOUTIO,'(/1X,''+++SAIRUN no SATR bank found'')')
        GOTO 900
      ENDIF
      KSA=KSATR+LMHLEN
C     link to SLAY bank
      KSLAY=IW(NAMIND('SLAY'))
      IF(KSLAY.EQ.0) THEN
        WRITE(LOUTIO,'(/1X,''+++SAIRUN no SLAY bank found'')')
        GOTO 900
      ENDIF
      NROW=LROWS(KSLAY)
      NCOL=LCOLS(KSLAY)
C     fill constants in common SATRCO
      DO 210 I=1,NROW
        PHIBSA(I)=RTABL(KSLAY,I,5)
  210 CONTINUE
      NLAYSA=IW(KSA+6)
      NSECSA=IW(KSA+7)
      NBRASA=IW(KSA+8)
      RMINSA=RW(KSA+11)
      RMAXSA=RW(KSA+12)
      BRTHSA=RW(KSA+13)
      DEADSA=RW(KSA+29)
      DGASSA=RW(KSA+31)
      NCRASA=IW(KSA+35)
      NCARSA=IW(KSA+36)
      NTDCSA=IW(KSA+37)
      NSIDSA=2
C     init counter variables
      NHITSA=0
      NLOSSA=0
      XHITSA=0.
      XLOSSA=0.
      XWIRSA=0.
      XEVTSA=0.
      DO 10 I1=1,9
        NHLASA(I1)=0
  10  CONTINUE
      DO 20 I2=1,9
        NHLDSA(I2)=0
  20  CONTINUE
C
C  Refer to output banks  only if SET SATR required
C
      IF (IDETJO(6).NE.0) THEN
          NASAHT=NAMIND('SAHT')
          NASADI=NAMIND('SADI')
          CALL BKFMT('SAHT','2I,(I)')
          CALL BKFMT('SADI','2I,(I)')
      ENDIF
 900  CONTINUE
      END
