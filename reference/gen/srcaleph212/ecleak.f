      SUBROUTINE ECLEAK( METH , STAC , ANGL , ICHA , ECOR )
C ----------------------------------------------------------------------
C!  Cluster corrected energy for longitudinal and transverse leakage.
C   AUTHOR   : J.Badier    15/06/89
C                          10/10/89
C                           1/05/91  ECLEAK
C!   Modified :- E. Lancon             23-OCT-1991
C!              Load ECIL to apply energy loss correction to
C!              charged clusters
CKEY PHOTONS LEAKAGE / INTERNAL
C   ECLEAK is a modified version of EBLEAK , with a new argument : ICHA
C
C   Input : METH        Correction option.
C           STAC(3)     Stacks content.
C           ANGL(2)     Polar and azimuthal angles in radians.
C           ICHA        Charge : 0 for a neutral cluster.
C
C   Output: ECOR        Corrected energy.
C
C   BANKS :
C     INPUT   : ECLK,NR=0  Correction parameters.
C               EGTH,NR=0
C               ECIL,NR=run#
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Calls none.
C   Called by ECOENT
C-----------------------------------------------------
C       The bank ECLK gives the estimated leakage as a function of the
C       logarithm of the energy with the bining define by BIN and as a
C       function of COSI /.7,.85,1./.
C             COSI =  cos( incidence angle )
C  1/5/91 :
C        6 columns per row : 1 , 2 , 4 , 8 , 16 , 32 Gev
C        12 rows : 4 groups of 3 incidence.
C
C                              |   COSI =  sin(theta)  =  .7
C                   | Barrel   |   COSI =  sin(theta)  =  .85
C                   |          |   COSI =  sin(theta)  = 1.
C   Neutral cluster |
C                   |          |   COSI = |cos(theta)| =  .7
C                   | Endcap   |   COSI = |cos(theta)| =  .85
C                              |   COSI = |cos(theta)| = 1.
C
C                              |   COSI =  sin(theta)  =  .7
C                   | Barrel   |   COSI =  sin(theta)  =  .85
C                   |          |   COSI =  sin(theta)  = 1.
C   Charged cluster |
C                   |          |   COSI = |cos(theta)| =  .7
C                   | Endcap   |   COSI = |cos(theta)| =  .85
C                              |   COSI = |cos(theta)| = 1.
C
      PARAMETER ( KBIN = 6 , KCOS = 3 , KRAG = KCOS * 4 )
      DIMENSION STAC(*) , ANGL(*) , SLIM(6)
      DIMENSION BIN(KBIN) , PRLK(KBIN,KRAG) , BCOS(KCOS)
      DIMENSION COR(3) , BRLK(2)
C   EMIN , EMAX : Energy range. EPRG : Factor for the energy bining.
C   DCOS : cos(i) bining.
      PARAMETER( EMIN = .5 , EMAX = 64. , EPRG = 2. , DCOS = .15 )
      PARAMETER( BLK1 = 1.61 , BLK2 = .51 , BLK3 = 2.2 )
      PARAMETER(JEGTT1=1,JEGTT2=2,JEGTT3=3,JEGTT4=4,JEGTT5=5,JEGTT6=6,
     +          LEGTHA=6)
      PARAMETER(JECIRN=1,JECIC1=2,JECIC2=3,JECILA=3)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      SAVE
      INTEGER ALGTDB,GTSTUP
      CHARACTER DET*2, LIST*4
      DATA IROLD/0/, DET/'EC'/, LIST/'ECIL'/
      DATA NAECIL/0/
C!    set of intrinsic functions to handle BOS banks
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
      ECOR = 1.
      IF( METH .EQ. 1 ) GO TO 98
C
C   First call.
      IF( NAECIL .EQ. 0 ) THEN
        NAECIL = NAMIND('ECIL')
        LDBAS = JUNIDB(0)
C
C   The PRLK content is taken into ECLK.
        KECLK = MDARD (IW,LDBAS,'ECLK',0)
        IF( KECLK .EQ. 0 ) GO TO 98
        DO 12 I = 1 , KRAG
          DO 11 J = 1 , KBIN
            PRLK( J , I ) = RTABL( KECLK , I , J )
   11     CONTINUE
   12   CONTINUE
C
C   Subcomponents theta limits from EGTH.
        KEGTH = MDARD (IW,LDBAS,'EGTH',0)
        IF( KEGTH .EQ. 0 ) GO TO 98
        TH13 = RTABL( KEGTH , 1 , JEGTT3 )
        TH16 = RTABL( KEGTH , 1 , JEGTT6 )
        TH23 = RTABL( KEGTH , 2 , JEGTT3 )
        TH26 = RTABL( KEGTH , 2 , JEGTT6 )
C
C   Initialisations.
C   Bining (.5,1.,2.,......,32.,64.)
        DELT = ALOG(EPRG)
        BIN(1) = 0.
        DO 13 I = 2 , KBIN
          BIN(I) = BIN(I-1) + DELT
   13   CONTINUE
        BCOS(KCOS) = 1.
        DO 14 I = KCOS-1 , 1 , -1
          BCOS(I) = BCOS(I+1) - DCOS
   14   CONTINUE
      ENDIF
C
C - next entry
C
      ENER = STAC(1) + STAC(2) + STAC(3)
C   Energy out of range.
      IF( ENER .LT. EMIN .OR. ENER .GT. EMAX ) GO TO 98
      ENLG = ALOG( ENER )
C
C   Search cosinus of incidence and subdetector.
      IF( ANGL(1) .LE. TH13 ) THEN
C   End cap A
        COSI = COS( ANGL(1) )
        IDT = KCOS - 2
      ELSE
        IF( ANGL(1) .GE. TH23 ) THEN
C   End cap A
          COSI = - COS( ANGL(1) )
          IDT = KCOS - 2
        ELSE
          IF( ANGL(1) .LT. TH16 .OR. ANGL(1) .GT. TH26 ) GO TO 1
C   Barrel
          COSI = SIN( ANGL(1) )
          IDT = - 2
        ENDIF
      ENDIF
C    Searches interpolation intervals.
      I0 = INT( ENLG / DELT + 1.5 )
      IF( I0 .LE. 1 ) I0 = 2
      IF( I0 .GE. KBIN ) I0 = KBIN - 1
C   Normalised distance to the bin I0.
      ECAR = ( ENLG - BIN(I0) ) / DELT
      ECA2 = ECAR ** 2
C   In this version KCOS = 3 and IC = 1 , 3 or 4 , 6 .
      I0C = 2
C   LC, 0=Barrel, 3=End-Cap
      LC = I0C + IDT
      IF ( ICHA .NE. 0 ) LC = LC + 6
      DO 4 I = 1 , 3
        IC = LC + I
        CM = PRLK( I0 - 1 , IC )
        C0 = PRLK( I0 , IC )
        CP = PRLK( I0 + 1 , IC )
C   Quadratic interpolation
        COR(I) = C0 + ECAR * ( .5 * ( CP - CM ) ) +
     +                ECA2 * ( .5 * ( CP + CM ) - C0 )
    4 CONTINUE
C   Quadratic interpolation coefficients.
      XCOS = ( COSI - BCOS(I0C) ) / DCOS
      XCS2 = XCOS ** 2
C   Quadratic interpolation.
      CORR = COR(2) + XCOS * ( .5 * ( COR(3) - COR(1) ) ) +
     +               XCS2 * ( .5 * ( COR(3) + COR(1) ) - COR(2) )
C
C?   Now for charged clusters apply ionisation loss correction
C    get ECIL bank from data base
C! Get banks from DB depending on run and setup code
C
      CALL ABRUEV (IRUN,IEVT)
      IRET = 0
      IF (IRUN.NE.IROLD) THEN
        IROLD = IRUN
        IF (IRUN.LE.2000) THEN
           ITP = GTSTUP (DET,IRUN)
        ELSE
           ITP = IRUN
        ENDIF
        IRET= ALGTDB(JUNIDB(0),LIST,-ITP)
      ENDIF
C
      IREG = LC - 6
      KECIL = IW(NAECIL)
      IF (KECIL.GT.0) THEN
        NECIL = LROWS(KECIL)
        IF ( ICHA.NE.0  ) THEN
          DO 5 IECIL = 1 ,  NECIL
            IF (ITABL(KECIL, IECIL, JECIRN).EQ.IREG) THEN
              CORR = CORR + RTABL(KECIL, IECIL, JECIC1)/MAX(ENER,0.001)
            ENDIF
    5     CONTINUE
        ENDIF
      ENDIF
C
C?  All Done
C
      IF ( CORR.LT.0.) CORR =0.
      ECOR = 1. + CORR
      GO TO 98
    1 CONTINUE
C  Overlap ( from P.Marotte ).
      CALL EBSLIM( ANGL(1) , ANGL(2) , SLIM , IER )
      IF( IER .GT. 0 ) GO TO 98
      TOTO = SLIM(6) / ( BLK1 + BLK2 * ENLG )
      ECOR = 1. + EXP( BLK3 - TOTO )
   98 CONTINUE
      RETURN
      END