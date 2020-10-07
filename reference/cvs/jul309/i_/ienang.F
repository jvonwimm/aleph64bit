      SUBROUTINE IENANG(IH,IA,JH,JA,ENI,ENJ)
C-----------------------------------------------------------------------
C! ITC: Calculate angle through cell.
C!
C!    Author      :- J. Sedgbeer 88/03/29
C!    Modified    :- I. Tomalin  89/02/28
C!
C!   Input:
C!      IH    /I  : Coord number (row) in ITCO
C!      IA    /I  : Ambiguity : 0 => -ve, -1 => +ve
C!      JH    /I  : Coord number (row) in ITCO
C!      JA    /I  : Ambiguity : 0 => -ve, -1 => +ve
C!
C! N.B. If coord. JH lies outside coord IH, then this subroutine will
C! assume that the track is going outwards and give ENI and ENJ approx 0.
C! However, if this is not the case, then it will assume that it is going
C! inwards, and will produce ENI and ENJ approx pi.
C!
C!      commons:     /BCS/
C!                   /ITNAMC/
C!      params.:     ALCONS,ITCOJJ
C!
C!   Output:
C!      ENI   /R  : Angle at which line IH-JH crosses layer IH
C!      ENJ   /R  : Angle at which line IH-JH crosses layer JH
C!
C!   calls:  None
C!
C!   Libraries required: none
C!
C!======================================================================
#ifndef DOC
C I/O Commons etc.
      INTEGER NITCO
      DATA    NITCO / 0 /
#include "bcs.h"
#include "alcons.h"
#include "itcojj.h"
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF ( NITCO .eq. 0 ) THEN
        NITCO = NAMIND( 'ITCO' )
      ENDIF
      KITC1 = IW( NITCO )
      IF ( KITC1        .NE. 0 .AND.
     &     IW(KITC1-2)  .LT. 1 ) THEN
        KITC1 = IW(KITC1-1)
      ENDIF
C
      PHI = RTABL(KITC1,IH,JITCP2+IA)
      PHJ = RTABL(KITC1,JH,JITCP2+JA)
      RLAYI  = RTABL(KITC1,IH,JITCRA)
      RLAYJ  = RTABL(KITC1,JH,JITCRA)
      XI  = RLAYI*COS(PHI)
      XJ  = RLAYJ*COS(PHJ)
      YI  = RLAYI*SIN(PHI)
      YJ  = RLAYJ*SIN(PHJ)
C
      DX = XJ - XI
      DY = YJ - YI
      D  = SQRT(DX**2 + DY**2)
C
      CENI = (DX*XI + DY*YI)/(RLAYI*D)
      CENJ = (DX*XJ + DY*YJ)/(RLAYJ*D)
C
      IF(CENI.GT. 1.) CENI =  1.
      IF(CENI.LT.-1.) CENI = -1.
      IF(CENJ.GT. 1.) CENJ =  1.
      IF(CENJ.LT.-1.) CENJ = -1.
C
C Find sign of entrance angle (being careful about 0-2pi boundary).
                                               SG = -1.0
      IF (MOD(PHJ-PHI+5.0*PI,TWOPI)-PI.GT.0.0) SG = 1.0
      ENI = SG*ACOS(CENI)
      ENJ = SG*ACOS(CENJ)
C
      END
#endif
