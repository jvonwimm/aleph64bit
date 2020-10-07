      SUBROUTINE ITDRIF(IL,IWIRE,ITDC,RSW,ZSW,SIGZ,
     *                   DIST1,DIST2,TIME,SIGRP)
C-----------------------------------------------------------------------
C! Get approx. drift dist. and error.
CKEY IPREDATA ITC /INTERNAL
C!
C!    Author     :- J. Sedgbeer
C!    Modified   :- J. Sedgbeer  90/01/04 Add check for neg. dist.
C!                                   Correct channels read at B end
C!    Modified   :- J. Sedgbeer  91/01/07 Ensure IWST bank O.K.
C!    Modified   :- J. Sedgbeer  01/07/93 Use function IDDIST for
C!                                       Drift-distance calc.
C!    Modified   :- J. Sedgbeer  02/04/95 Bunch-trains: modify calc.
C!                               of time from TDC value to use bunch
C!                               number and spacing in /IBUNCC/
C!    Modified   :- J. Sedgbeer  10/06/95. Protect against very large
C!                               drift-times in data with bunch trains.
C!
C!    Input:
C!      IL     /I  : ITC layer number
C!      IWIRE  /I  : ITC wire number [1,960]
C!      ITDC   /I  : TDC value
C!      RSW    /R  : Radius (cm.)
C!      ZSW    /R  : Z coord. (cm.)
C!      SIGZ   /R  : sigma(Z) (cm.)
C!      need commons     /ITWICC/ ITC wire geom.
C!                       /IRFECC/ R-phi front-end constants.
C!                       /IDRPCC/ Drift-time relation coeffs.
C!                       /IRESCC/ Resolution vs cell position coeffs.
C!                       /BCS/    For IWST, IEDD and IET0 banks
C!                       /IBUNCC/ For bunch train info.
C!           parameters  ALCONS
C!                       IEDDJJ
C!                       IET0JJ
C!                       IWSTJJ
C!
C!    Output:
C!      DIST1  /R  : Azimuthal dist.of +ve hit from sense wire (cm.)
C!                                       DIST1 is positive
C!      DIST2  /R  : Azimuthal dist.of -ve hit from sense wire  (cm.)
C!                                       DIST2 is negative
C!      TIME   /R  : Drift time    (ns.)
C!      SIGRP  /R  : Sigma R-Phi   (cm.)
C!                         > 0   if O.K.
C!                         < 0   if TDC or time value out of O.K. range
C!
C!    called by : ICRCOO (Alephlib)
C!    calls     : IDDIST (Alephlib)
C!    Libraries required : none
C!
C! This routine is part of the ITC Alephlib routines to make coords.
C! from digis.  See ICRCOO. Initialisation must be done - see ICRCOO.
C!
C?  Convert TDC value to time.
C?  Correct time for TOF and signal propagation.
C?  Correct for fine tune time offsets (per layer and wire)
C?  Calculate drift distance from time - use function IDDIST
C?  Correct drift distance
C?  Set Sigma(R-Phi)
C?  If Z coord. bad (or unknown) adjust Sigma(R-Phi)
C-----------------------------------------------------------------------
      SAVE
C I/O commons etc.
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JIWSID=1,JIWSVR=2,JIWSIW=4,JIWSFL=5,JIWSIP=6,LIWSTA=6)
      PARAMETER(JIEDID=1,JIEDVR=2,JIEDLN=4,JIEDNB=5,JIEDBW=6,JIEDBL=7,
     +          JIEDBH=8,JIEDDD=9,LIEDDA=58)
      PARAMETER(JIETID=1,JIETVR=2,JIETLN=4,JIETNW=5,JIETDT=6,LIET0A=149)
      PARAMETER (JWIRIT=8,MWIRIT=960)
      COMMON/ITWICC/RWIRIT(JWIRIT),NWIRIT(JWIRIT),IWIRIT(JWIRIT),
     +              PHWRIT(JWIRIT),CELWIT(JWIRIT),WZMXIT,SGMXIT
      INTEGER MLAYID,MCOFID
      REAL TTOFID,DTCOID
      PARAMETER (MLAYID=8,MCOFID=5)
      COMMON/IDRPCC/TTOFID(MLAYID),DTCOID(MCOFID,MLAYID)
      INTEGER JLAYIR,IBN0IR,ITLOIR,ITHIIR
      REAL BWIDIR
      PARAMETER (JLAYIR=8)
      COMMON/IRFECC/BWIDIR,IBN0IR,ITLOIR(JLAYIR),ITHIIR(JLAYIR)
      INTEGER JCOFIR,JLAYRE
      REAL RESCOF
      PARAMETER (JCOFIR=3,JLAYRE=8)
      COMMON/IRESCC/RESCOF(JCOFIR,JLAYRE)
      INTEGER IBUNCH
      REAL TBUNCH
      COMMON/IBUNCC/IBUNCH,TBUNCH
      REAL DCOR(2),IDDIST,DDDT
      INTEGER IBAD
      EXTERNAL NAMIND,IDDIST
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
      IF(FIRST) THEN
        NIEDD = NAMIND('IEDD')
        NIET0 = NAMIND('IET0')
        NIWST = NAMIND('IWST')
        FIRST = .FALSE.
      ENDIF
C
C Check TDC value
C
      SIGRP = -1.
      IF(ITDC.LE.ITLOIR(IL).OR.ITDC.GT.ITHIIR(IL)) GOTO 999
C
C Calculate Time from TDC value.
C
      TIME = BWIDIR*FLOAT(IBN0IR - ITDC)
C
C Correct for bunch number (if no bunch-trains then IBUNCH=1)
C
      TIME = TIME - (IBUNCH-1)*TBUNCH
C
C Correct for TOF and signal propagation,TWIR.
C Note that the TWIR correction is symmetric about z=0, the propagation
C time for z=0 is absorbed (approximately) into the zero bin IBN0IR.
C Adjust TWIR for Channels read at B end (use info. in IWST bank).
C Corrections will be wrong when the Z coord. is bad (in this case Z is
C set to zero with a large error). The errors will be ~ 1ns in TOF
C and ~ 1ns in TWIR. This is significant,therefore in this case the
C error on the coord. is made larger - see below.
C
      TOF = SQRT(RSW**2+ZSW**2)/CLGHT
      TWIR= -ZSW/CLGHT
      KIWST = IW(NIWST)
      IF(KIWST.GT.0) THEN
        NW = LROWS(KIWST)
        DO 50 I=1,NW
          KK = KROW(KIWST,I)
          IF(IWIRE.EQ.IW(KK+JIWSIW).AND.IW(KK+JIWSFL).EQ.3) TWIR=-TWIR
   50   CONTINUE
      ENDIF
      TIME= TIME-TOF-TWIR
C
C Correct for fine tune timing offset.
C
      TIME = TIME + TTOFID(IL)
C
C Correct for individual wire T0's (IET0 bank)
C
      KIET0 = IW(NIET0)
      IF(KIET0.GT.0) THEN
        IWLN = IWIRE - IWIRIT(IL)
        TIME = TIME - RTABL(KIET0,IL,JIETDT-1+IWLN)
      ENDIF
C
C Don't allow negative times. If very negative set bad coord flag.
C                             (Mean drift-veloc ~ 30 microns/ns)
      IBAD = 0
      IF(TIME.LT.-10.0) IBAD = 1
      IF(TIME.LT.  0.0) TIME = 0.0
C
C Calculate distance from time (use drift-relation)
C
      DIST = IDDIST(IL,TIME,DDDT)
      DIST = AMAX1(0.,DIST)
      IF(DIST.GE.CELWIT(IL)) IBAD = 1
      FRAC = DIST/(0.5*CELWIT(IL))
C
C Correct the distance using look-up table in IEDD bank
C                         (not for bad coords.)
      DCOR(1) = 0.
      DCOR(2) = 0.
C
      IF(IBAD.EQ.1) GOTO 200
C
      KIEDD = IW(NIEDD)
      IF(KIEDD.GT.0) THEN
        KK = KROW(KIEDD,IL)
        NB = IW(KK+JIEDNB)
        IF(NB.GT.1) THEN
          BL = RW(KK+JIEDBL)
          BH = RW(KK+JIEDBH)
          BW = RW(KK+JIEDBW)
          DO 100 J=1,2
            DD = FRAC
            IF(J.EQ.2) DD = -FRAC
            IF (DD.LE.BL) THEN
              DCOR(J) = RW(KK+JIEDDD)
            ELSE IF (DD.GE.BH) THEN
              DCOR(J) = RW(KK+JIEDDD-1+NB)
            ELSE
              I = (DD-BL)/BW + 1
              D = DD - BL - (I-1)*BW
              DCOR(J) = RW(KK+JIEDDD-1+I) +
     +                 (RW(KK+JIEDDD+I) - RW(KK+JIEDDD-1+I))*D/BW
            ENDIF
  100     CONTINUE
        ENDIF
      ENDIF
C
  200 CONTINUE
      DIST1 =  AMAX1(0.,( DIST - DCOR(1)))
      DIST2 =  AMIN1(0.,(-DIST + DCOR(2)))
C
C Set Sigma r-phi - depends on fractional dist. across cell.
C
      SIGRP = RESCOF(1,IL) + RESCOF(2,IL)*FRAC + RESCOF(3,IL)*FRAC**2
C
C If Z coord. bad then make sigma(r-phi) larger.
C Note the approx uncertainty on the drift-distance caused by not
C knowing z (differentiate expression for DIST and assume delta t = 2ns)
C If DDDT bad then use nominal values.
C
      IF(SIGZ.GT.100.) THEN
        IF(DDDT.GT.0.) THEN
          UDIST = 2.0*DDDT
        ELSE
          UDIST = 0.0060
        ENDIF
        SIGRP = SQRT(SIGRP**2 + UDIST**2)
      ENDIF
C
C If rogue time (coord.) set resolution large
C
      IF(IBAD.EQ.1) SIGRP = 1.0
C
  999 CONTINUE
      END
