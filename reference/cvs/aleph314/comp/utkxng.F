      SUBROUTINE UTKXNG(HP,COSTHN,ZLYR)
C-----------------------------------------------------------------------
CKEY COMPUTE FIT
C! Routine to compute track crossings of the ITC sense wire planes
C! This routine has some similarity to IXHITS by I. Tomalin.
C!
C!    Author  :  W. B. Atwood  14-Oct-89
C!
C!   Input:
C!     HP(5)  /R4 : Track helix parameters, as in bank FRFT
C!     need common /ITWICC/ for ITC geom.
C!
C! IMPORTANT: If you wish to use this subroutine, then at the beginning
C!            of each run, you must call the ALEPHLIB routine, IRDDAF,
C!            to fill commons with the ITC geometry etc.
C!
C!   Output:
C!     XTK    /R4 : Array of space points for each crossing.
C!                  (dimension... (3,8))
C!     TTK    /R4 : Array of direction cosines.. for each XTK
C!
C!   calls     : none
C!
C!   Libraries required: BOS
C!
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "itwicc.h"
#include "alcons.h"
C
C-----------------------------------------------------------------------
      DIMENSION COSTHN(*),ZLYR(*),HP(*)
C-----------------------------------------------------------------------
C
      DO 5 LAY = 1,8
        COSTHN(LAY)=1.
        ZLYR(LAY)=0.
    5 CONTINUE
C
C Get the parameters of the track
C
      R0 = 1.0/HP(1)
      TANL = HP(2)
      PHI0 = HP(3)
      D0 = HP(4)
      D0Q   = D0*D0
      Z0 = HP(5)
      SINTH = 1./SQRT(1.+TANL**2)
      COSTH = SINTH * TANL
C
C Loop over the layers of the ITC and find their intersection points    h
C                          with the track.
      DO 50 LAY = 1,8
        RAD   = RWIRIT(LAY)
C Check that there is an intersection.
        IF (ABS(D0).GT.RAD.OR.ABS(2.0*R0-D0).LT.RAD) GOTO 50
        RADQ  = RAD*RAD
        FACT1 = RADQ - D0Q
        FACT2 = 0.5*FACT1/(R0-D0)
C Get the phi coordinate of the intercept.
        SINA  = (FACT2-D0)/RAD
        IF(ABS(SINA).GT.1.0) GOTO 50
        PHIT = PHI0 + ASIN(SINA)
C Get the z coordinate of the intersept.
        SPSI2 = 0.5*FACT2/R0
        IF (SPSI2.LT.0.0.OR.SPSI2.GT.1.0) GOTO 50
        PSI = ASIN(SQRT(SPSI2))
        ZT  = Z0 + 2.0*PSI*ABS(R0)*TANL
        IF(ABS(ZT).GT.WZMXIT) GOTO 50
        ZLYR(LAY)=ZT
C
C Get the angle between track and wire plane
        PHITRK = PHI0 + 2.*SIGN(PSI,R0)
        COSTHN(LAY)=ABS(COS(PHITRK-PHIT))
   50 CONTINUE
      RETURN
      END
#endif
