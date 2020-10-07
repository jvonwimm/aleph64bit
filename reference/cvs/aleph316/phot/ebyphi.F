      SUBROUTINE EBYPHI( YCO , PHI )
C ----------------------------------------------------
C   AUTHOR   : R.Clifft 08/06/88
C               J.Badier   29/11/89
C! Convert local coordinate y ,orthogonal to a crack, to ALEPH
C! coordinate phi.
CKEY PHOTONS CRACK COORDINATE / INTERNAL
C
C   Input  :    YCO  Local coordinate.
C
C   Output    :    PHI  ALEPH coordinate phi.
C
C     called by      EBRANC
C     calls          NONE
C
C     banks          NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
C            DISB = ECAL barrel radius parameter
C            PHIH = ECAL module half phi angle
C            PHIO = ECAL barrel - end cap phi offset.
      PARAMETER( PHIO = .0327 , PHIH = .2618 )
      PARAMETER( DISB = 186.8 , DISF = .26795 , YTMP = 2.5 )
#include "alcons.h"
#include "ebenec.h"
      IF( NREGEB(1) .NE. 2 ) THEN
C
C *** PHI from Y endcap
C
        JR = 2
        IF( KODEEB(3) .EQ. NREGEB(1) .OR.
     +      KODEEB(3) .EQ. NREGEB(1) + 1 )   JR = 1
        YTEMP = 3. - YLIMIT(JR)
        PHICR = FLOAT( NREGEB(2) - 1 ) * 2. * PHIH -
     +          SIGN( PHIH , YTEMP ) - PHIO
        YDIF = SIGN( YCO , YTEMP )
        SIHARG = ( FLOAT(ITRWEB) - .5 ) * CECT1 + CECT2
        HDIST = DISFE * ( EXP(SIHARG) - EXP(-SIHARG) ) / 2.
        PHI = PHICR + ATAN2( YDIF , HDIST )
        IF(PHI .LT. 0.) PHI = PHI + TWOPI
C
      ELSE
C
C *** PHI from Y barrel
C
        PHIMD = ( FLOAT(NREGEB(2)) - .5 ) * 2. * PHIH - PHIO
        YDIF = DISB * DISF - YCO
        YTEMP = YTMP - FLOAT(KODEEB(3))
        YDIF = SIGN( YDIF , YTEMP )
        PHI = PHIMD - ATAN2( YDIF , DISB )
C
      ENDIF
      RETURN
      END
#endif
