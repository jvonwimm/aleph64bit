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
#ifndef DOC
      SAVE
C I/O commons etc.
#include "alcons.h"
#include "bcs.h"
#include "iwstjj.h"
#include "ieddjj.h"
#include "itwicc.h"
#include "irescc.h"
      REAL DCOR(2),IDDIST,DDDT
      EXTERNAL NAMIND,IDDIST
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
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
#endif
