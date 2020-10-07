      REAL FUNCTION SIDCOR(ISTAK,ESUM)
C----------------------------------------------------------------------
C!  - Returns dead pad missing energy in cluster (SANDWICH METHOD)
C!
C!   Author   :  B.Vallage June 93
C!
C!   Inputs:    Dead Pad layer number ISTAK
C!   Outputs:   Dead Pad energy SIDCOR
C!---------------------------------------------------------------------
C?
C?   Description   Function to determine the energy of a known dead pad
C?   ===========   within a cluster from an exprapolation using the
C?                 shower shape.
C?                 In each of the 12 layers , a parabolic fit has been
C?                 established giving the Energy sum ( over 3 Phi bin )
C?                 in an R bin , from the corresponding Energy sums in
C?                 the preceeding and following layers.
C?  If ESUM(1) is the energy sum in previous layer ,ESUM(3) in the next
C?  one , and A the Asysymetry variable
C?            (ESUM(1)-ESUM(3))/(ESUM(1)+ESUM(3))
C?  then :
C?  ESUM(2)/(ESUM(1)+ESUM(3)) = a . A**2  + b . A  + c
C?      where a,b,c depends on the layer #
C?  SIDCOR is the missing contribution to ESUM(2)
C!======================================================================
#ifndef DOC
C Sandwich correction for dead pads B.VALLAGE June 19th 1993
#include "bcs.h"
      REAL ESUM(3),ENESUM,ASYLAY
      INTEGER ISTAK
#include "sdcojj.h"
      DATA NASDCO /0/
#include "bmacro.h"
      IF ( NASDCO.LE.0) NASDCO = NAMIND('SDCO')
      KSDCO = IW(NASDCO)
      ENESUM = ESUM(1) + ESUM(3)
      IF(ENESUM.GT.0 .AND. KSDCO.GT.0 ) THEN
        AA = RTABL(KSDCO,ISTAK,JSDCEC)
        BB = RTABL(KSDCO,ISTAK,JSDCEC+1)
        CC = RTABL(KSDCO,ISTAK,JSDCEC+2)
        ASYLAY = (ESUM(1) - ESUM(3)) / ENESUM
        SIDCOR= MAX( 0.,
     & (( AA*ASYLAY**2+BB*ASYLAY+CC ) * ENESUM  - ESUM(2))  )
      ELSE
        SIDCOR = 0.
      ENDIF
C
      RETURN
      END
#endif
