      SUBROUTINE ICDRIF(IL,IWIRE,TIME,RSW,ZOLD,ZNEW,DIST1,DIST2,SIGRP)
C-----------------------------------------------------------------------
CKEY ITC
C! Get corrected drift dist. and error.
C!
C!    Author          :- W. Atwood  89/12/16.
C!    Modified        :- J.Sedgbeer 01/07/93 Use function IDDIST for
C!                                           Drift-distance calc.
C!    Input:
C!      IL     /I  : ITC layer number
C!      IWIRE  /I  : ITC wire no. [1,960]
C!      TIME   /I  : Drift time - fully corrected (ns)
C!      RSW    /R  : Sense wire radius
C!      ZOLD   /R  : Old value of z at which co-ord was evaluated
C!      ZNEW   /R  : New value of z at which co-ord will be evaluated
C!      need commons     /ITWICC/ ITC wire geom.
C!                       /IRESCC/ Resolution vs cell position coeffs.
C!                       /BCS/    For IWST and IEDD banks
C!           parameters  ALCONS
C!                       IEDDJJ
C!                       IWSTJJ
C!
C!    Output:
C!      DIST1  /R  : Azimuthal dist.of +ve hit from sense wire (cm.)
C!                                       DIST1 is positive
C!      DIST2  /R  : Azimuthal dist.of -ve hit from sense wire  (cm.)
C!                                       DIST2 is negative
C!      SIGRP  /R  : Sigma R-Phi   (cm.)
C!                         > 0   if O.K.
C!                         < 0   if TDC value out of O.K. range
C!
C!    calls     : IDDIST (Alephlib)
C!    Libraries required : none
C!
C! This routine is based on ITDRIF. ITDRIF makes a coord. (drift-dist)
C! from a digitising, i.e. before track info. is available. This routine
C! ICDRIF, corrects the drift distance the Z using info. from the track
C! trajectory.
C!
C?  Correct drift-time for TOF and signal propagation time at new Z
C?  Calculate drift distance from time - use function IDDIST
C?  Correct drift distance
C?  Set Sigma(R-Phi)
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
      PARAMETER (JWIRIT=8,MWIRIT=960)
      COMMON/ITWICC/RWIRIT(JWIRIT),NWIRIT(JWIRIT),IWIRIT(JWIRIT),
     +              PHWRIT(JWIRIT),CELWIT(JWIRIT),WZMXIT,SGMXIT
      INTEGER JCOFIR,JLAYRE
      REAL RESCOF
      PARAMETER (JCOFIR=3,JLAYRE=8)
      COMMON/IRESCC/RESCOF(JCOFIR,JLAYRE)
      REAL DCOR(2),IDDIST,DDDT
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
        NIWST = NAMIND('IWST')
        FIRST = .FALSE.
      ENDIF
C
C Correct time to new Z location: correct TOF and signal time down
C wire, TWR. Adjust TWR for Channels read at B end (use info. in IWST
C bank).
C
      TOFO = SQRT(RSW**2+ZOLD**2)/CLGHT
      TWRO = -ZOLD/CLGHT
      TOFN = SQRT(RSW**2+ZNEW**2)/CLGHT
      TWRN = -ZNEW/CLGHT
C
      KIWST = IW(NIWST)
      IF(KIWST.GT.0) THEN
        NW = LROWS(KIWST)
        DO 50 I=1,NW
          KK = KROW(KIWST,I)
          IF(IWIRE.EQ.IW(KK+JIWSIW).AND.IW(KK+JIWSFL).EQ.3) THEN
            TWRO = -TWRO
            TWRN = -TWRN
          ENDIF
   50   CONTINUE
      ENDIF
      TIME = TIME - (TOFN+TWRN -TOFO-TWRO)
C
C Calculate distance from time (use drift-relation)
C
      DIST = IDDIST(IL,TIME,DDDT)
      DIST = AMAX1(0.,DIST)
      FRAC = DIST/(0.5*CELWIT(IL))
C
C Correct the distance using look-up table in IEDD bank
C
      DCOR(1) = 0.
      DCOR(2) = 0.
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
      DIST1 =  AMAX1(0.,( DIST - DCOR(1)))
      DIST2 =  AMIN1(0.,(-DIST + DCOR(2)))
C
C Set Sigma r-phi - depends on fractional dist. across cell.
C
      SIGRP = RESCOF(1,IL) + RESCOF(2,IL)*FRAC + RESCOF(3,IL)*FRAC**2
C
  999 CONTINUE
      END
