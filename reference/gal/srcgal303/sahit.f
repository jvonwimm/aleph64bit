      SUBROUTINE SAHIT
C-----------------------------------------------------------------------
C!    calculate hit and fill SAHI bank
C                                   H.Burkhardt   October 1986
C                       modified by E.Neugebauer  December 1989
C                         (introduce alignment corrections)
C!    Modified:   H. Meinhard       03-Aug-1990  (1)
C     (read alignment constants from LALI)
C
C      called by GUSTEP                      from this .HLB
C      when track is in SATR acceptance
C-----------------------------------------------------------------------
      SAVE
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
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER(LCSAHT=1,MXSAHT=500,LCSADI=1)
      COMMON/SANAMC/NASAHT,NASADI
      COMMON/SATRCO/NSECSA,NCRASA,NCARSA,NTDCSA,NSIDSA,NLAYSA,NBRASA,
     .      RMINSA,RMAXSA,BRTHSA,ZLUMSA,DEADSA,DGASSA,COSTSA,
     .      NHITSA,NLOSSA,NWIRSA,XHITSA,XLOSSA,XWIRSA,XEVTSA,
     .      NHLASA(9),NHLDSA(9),PHIBSA(9)
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
C
C shifts of modules 1 to 4 due to alignment corrections:
      DIMENSION DMX(4), DMY(4), DMZ(4)
C
C rotations of modules 1 to 4 due to alignment corrections:
      DIMENSION ROX(4), ROY(4), ROZ(4)
C
C rotation, position of track element, result of cross product:
      DIMENSION ROT(3), POT(3), DRO(3)
C
C shifted track elements TRKNXT and TRKELE:
      DIMENSION TRKN(3), TRKE(3)
C
C nominal z-position of LCAL front:
      DATA ZNOM / 262.5 /
C
      LOGICAL DEBUG, LFIRST
      DATA LFIRST / .TRUE. /
C
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
C
C initialisation: get the alignment constants
C
      IF ( LFIRST ) THEN
C
         LFIRST = .FALSE.
         KLALI = IW( NAMIND( 'LALI' ) )
         IF ( KLALI .EQ. 0 )                                GOTO 901
         DO 100 I = 1, 4
            DMX( I ) = RTABL( KLALI, 5-I, 4 )
            DMY( I ) = RTABL( KLALI, 5-I, 5 )
            DMZ( I ) = RTABL( KLALI, 5-I, 6 )
            ROX( I ) = - RTABL( KLALI, 5-I, 7 )
            ROY( I ) = - RTABL( KLALI, 5-I, 8 )
            ROZ( I ) = - RTABL( KLALI, 5-I, 9 )
  100    CONTINUE
C
      ENDIF
C
C     digitize only when particle is charged and
C     leaving the detector
      IF(ITRKEL(8).NE.2.) GOTO 901
      IF(FBEGJO(6)) THEN
         DEBUG = FDEBJO .AND. ICSAJO(1).GT.0
C       initialize SAHIt bank for new event
        FBEGJO(6)=.FALSE.
C       count the number of events which hit the SATR
        XEVTSA=XEVTSA+1.
C       make sure no bank exits (for example read from INPUT)
        CALL BDROP(IW,'SAHT')
C       book new bank, maximum length is MXSAHT columns
         CALL ALBOS ('SAHT',0,LCSAHT*MXSAHT+LMHLEN,KSAHT,IGARB)
        CALL BLIST(IW,'E+','SAHT')
C       define the columns length
        IW(KSAHT+1)=LCSAHT
C       start with 0 rows
        IW(KSAHT+2)=0
      ENDIF
C
C     average between TRKELE and TRKNXT to get the
C     coordinates at the middle of the path through the
C     chamber
      X=0.5*(TRKNXT(1)+TRKELE(1))
      Y=0.5*(TRKNXT(2)+TRKELE(2))
      Z=0.5*(TRKNXT(3)+TRKELE(3))
C     go from x,y,z to cylindrical system with r and phi,theta
      RTOT=SQRT(X**2+Y**2+Z**2)
      RXY=SQRT(X**2+Y**2)
      PHI=ATAN2(Y,X)
      IF(PHI.LT.0.) PHI=PHI+TWOPI
C     calculate isid,ilay,isec,iwir from position
      IF(Z.GE.0.) THEN
        ISID=1
      ELSE
        ISID=2
      ENDIF
      ZLUM=ABS(Z)-ZLUMSA
      ILAY=INT(ZLUM/1.9)+1
      ISEC=MOD(INT( (PHI+TWOPI-PHIBSA(ILAY))/PIBY4 ),NSECSA)+1
      IF(DEBUG) THEN
        IF(RTOT.NE.0.) COST=Z/RTOT
        THETA=ACOS(COST)*RADEG
        PHID=PHI*RADEG
        WRITE(LOUTIO,'(/1X,''+++SAHIT+++ BEGIN OF DEBUG PRINT'',
     &   3X, ''VOLUME name and number:'',1X,A4,3X,I3,
     .    /''     RXY,RTOT='',F8.4,F10.4,
     .      ''    ZPOS relativ to SATR window='',F8.4,
     &   3X, ''COST, PHI = '',2F12.6)')
     &     TRKVOL,ITRKEL(6),RXY,RTOT,ZLUM,COST,PHI
      ENDIF
C
C get the module number:
C
      IF ( ISID .EQ. 1 ) THEN
         IF ( ISEC .LE. 4 ) THEN
            MODU = 1
         ELSE
            MODU = 2
         ENDIF
      ELSE
         IF ( ISEC .LE. 4 ) THEN
            MODU = 3
         ELSE
            MODU = 4
         ENDIF
      ENDIF
C
C rotation vector of this module:
C
      ROT( 1 ) = ROX( MODU )
      ROT( 2 ) = ROY( MODU )
      ROT( 3 ) = ROZ( MODU )
C
C shift the vector TRKNXT:
C
      POT( 1 ) = TRKNXT( 1 )
      POT( 2 ) = TRKNXT( 2 )
      POT( 3 ) = TRKNXT( 3 )
      IF ( POT( 3 ) .GT. .0 ) THEN
         POT( 3 ) = POT( 3 ) - ZNOM
      ELSE
         POT( 3 ) = POT( 3 ) + ZNOM
      ENDIF
      CALL CROSS( ROT, POT, DRO )
      TRKN( 1 ) = TRKNXT( 1 ) - ( DMX( MODU ) + DRO( 1 ) )
      TRKN( 2 ) = TRKNXT( 2 ) - ( DMY( MODU ) + DRO( 2 ) )
      TRKN( 3 ) = TRKNXT( 3 ) - ( DMZ( MODU ) + DRO( 3 ) )
C
C shift the vector TRKELE:
C
      POT( 1 ) = TRKELE( 1 )
      POT( 2 ) = TRKELE( 2 )
      POT( 3 ) = TRKELE( 3 )
      IF ( POT( 3 ) .GT. .0 ) THEN
         POT( 3 ) = POT( 3 ) - ZNOM
      ELSE
         POT( 3 ) = POT( 3 ) + ZNOM
      ENDIF
      CALL CROSS( ROT, POT, DRO )
      TRKE( 1 ) = TRKELE( 1 ) - ( DMX( MODU ) + DRO( 1 ) )
      TRKE( 2 ) = TRKELE( 2 ) - ( DMY( MODU ) + DRO( 2 ) )
      TRKE( 3 ) = TRKELE( 3 ) - ( DMZ( MODU ) + DRO( 3 ) )
C
C average between TRKE and TRKN to get the
C coordinates at the middle of the path through the
C chamber
      X = 0.5 * ( TRKN(1) + TRKE(1) )
      Y = 0.5 * ( TRKN(2) + TRKE(2) )
      Z = 0.5 * ( TRKN(3) + TRKE(3) )
C     go from x,y,z to cylindrical system with r and phi,theta
      RTOT=SQRT(X**2+Y**2+Z**2)
      RXY=SQRT(X**2+Y**2)
      PHI=ATAN2(Y,X)
      IF(PHI.LT.0.) PHI=PHI+TWOPI
C     calculate isid,ilay,isec,iwir from position
      IF(Z.GE.0.) THEN
        ISID=1
      ELSE
        ISID=2
      ENDIF
      ZLUM=ABS(Z)-ZLUMSA
      ILAY=INT(ZLUM/1.9)+1
      ISEC=MOD(INT( (PHI+TWOPI-PHIBSA(ILAY))/PIBY4 ),NSECSA)+1
      IF(DEBUG) THEN
        IF(RTOT.NE.0.) COST=Z/RTOT
        THETA=ACOS(COST)*RADEG
        PHID=PHI*RADEG
        WRITE(LOUTIO,'(/1X,''+++SAHIT+++ BEGIN OF 2. DEBUG PRINT'',
     &   3X, ''VOLUME name and number:'',1X,A4,3X,I3,
     .    /''     RXY,RTOT='',F8.4,F10.4,
     .      ''    ZPOS relativ to SATR window='',F8.4,
     &   3X, ''COST, PHI = '',2F12.6)')
     &     TRKVOL,ITRKEL(6),RXY,RTOT,ZLUM,COST,PHI
      ENDIF
C
C     checking on dead zones
C
C     rotate to a system where the sector starts at 0 degrees
C     to do that subtract the angle of the beginning of the sector
C     do the same for the end of the sector
      PHIRO=PHI+TWOPI-(PHIBSA(ILAY)+FLOAT(ISEC-1)*PIBY4)
      YR1=RXY*SIN(PHIRO)
      YR2=RXY*SIN(PHIRO-PIBY4)
      IF(YR1.LT.DEADSA.OR.YR2.GT.-DEADSA) THEN
      IF (DEBUG)
     .    WRITE(LOUTIO,'(''     Hit lost  ILAY,ISEC='',2I3,
     .      ''   left, right dist. to boundary='',2F12.5,
     .      ''   in edge region between sectors'')') ILAY,ISEC,YR1,YR2
        NLOSSA=NLOSSA+1
        XLOSSA=XLOSSA+1.
        GOTO 900
      ENDIF
C     sectors 1 and 5 start with a gas channel, 1.6 cm dead space
C     sectors 4 and 8 end   with a gas channel
      IF(((ISEC.EQ.1.OR.ISEC.EQ.5).AND.YR1.LT. DGASSA).OR.
     .   ((ISEC.EQ.4.OR.ISEC.EQ.8).AND.YR2.GT.-DGASSA)) THEN
      IF (DEBUG)
     .    WRITE(LOUTIO,'(''     Hit lost  ILAY,ISEC='',2I3,
     .      ''   left, right dist. to boundary='',2F12.5,
     .      ''   in gas channel'')') ILAY,ISEC,YR1,YR2
        NLOSSA=NLOSSA+1
        XLOSSA=XLOSSA+1.
        GOTO 900
      ENDIF
C
C     rotate to a system where the sector is symmetric to 0
C     in this system it is easy to get the distance wire, hit
      PHIRO=PHIRO-PIBY8
      POS=RXY*COS(PHIRO)
      DIST=POS-RMINSA
      IWIR=INT(DIST/BRTHSA)+1
C check if we are still in the sensitive area (may be not the
C case anymore due to alignment corrections!):
      IF ( IWIR .LT. 1 .OR. IWIR .GT. 14 ) GOTO 900
      DIST=ABS(AMOD(DIST,BRTHSA)-0.5*BRTHSA)
      NHITSA=NHITSA+1
      XHITSA=XHITSA+1.
      NHLASA(ILAY)=NHLASA(ILAY)+1
      IF (DEBUG)
     .  WRITE(LOUTIO,'(''     GOOD HIT IN ISID,ILAY,ISEC,IWIR='',4I5,
     .  '' DIST='',F12.5)')ISID,ILAY,ISEC,IWIR,DIST
C     now pack all the information about this hit in one word
      KSAHT=IW(NASAHT)
C     the dist is always between 0. and .5 cm, multiply with
C     2**16 to use the full 16 bit precision
      IDAT=ISHFT(ISID,28)+ISHFT(ILAY,24)+ISHFT(ISEC,20)+
     &     ISHFT(IWIR,16)+INT(DIST*65536.)
      IF(KSAHT.EQ.0) GOTO 900
      LCOL=LCOLS(KSAHT)
      LROW=LROWS(KSAHT)
      IF(LFRWRD(KSAHT).LT.LCOL) THEN
        NEW=IW(KSAHT)+100*LCSAHT
         CALL ALBOS ('SAHT',0,NEW,KSAHT,IGARB)
      ENDIF
      KSA=KNEXT(KSAHT)
C     fill in the next col  (in this case only a single hit)
      IW(KSA+1)=IDAT
C     increment number of columns
      IW(KSAHT+2)=IW(KSAHT+2)+1
C     exit for dead regions, no digitization, bank problems
  900 CONTINUE
  901 CONTINUE
      END
