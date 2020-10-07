      REAL FUNCTION IDDIST(IL,TIME,DDDT)
C-----------------------------------------------------------------------
CKEY ITC
C! Drift-distance calc. from drift-time relation.
C!
C!    Author    :- J.Sedgbeer 93/07/01
C!    Modified  :- J.Sedgbeer 95/06/10 Protect for v.large times beyond
C!                 reasonable input range of t-d relation.
C!
C!    Input:
C!      IL     /I  : ITC layer number
C!      TIME   /I  : Drift time - fully corrected (ns)
C!      need commons     /IDRPCC/ Drift-time relation.
C!                       /ITWICC/ ITC geom.
C!
C!    Output:
C!     IDDIST  /R  : Drift-distance (cm.). Max value = cell width.
C!     DDDT    /R  : dD/dt - gradient of drift-relation at time TIME
C!                         - set negative if v. large input time.
C!
C!    calls     :          none
C!    Libraries required : none
C!
C! Use drift relation coeffs. in /IDRPCC/ and TIME to calculate
C! the drift distance and the gradient dD/dt at time TIME.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C I/O commons etc.
#include "itwicc.h"
#include "idrpcc.h"
      INTEGER IL,I
      REAL TIME,DDDT,DIST
C-----------------------------------------------------------------------
C
      DIST = 0.0
      DDDT = 0.0
      DO 10 I=MCOFID,1,-1
        DDDT = DDDT*TIME + FLOAT(I)*DTCOID(I,IL)
        DIST = (DIST + DTCOID(I,IL)) * TIME
   10 CONTINUE
C
C Check distance and gradient:
C If grad. neg. and at edge of cell but dist small then set dist.
      IF(DDDT.LT.0.) THEN
        IF(TIME.GT.200. .AND.(DIST.LT.0.35*CELWIT(IL))) THEN
          DIST = CELWIT(IL)
        ENDIF
      ELSE
        IF(DIST.GT.CELWIT(IL)) THEN
          DIST = CELWIT(IL)
          DDDT = -DDDT
        ENDIF
      ENDIF
C
      IDDIST = DIST
C
      END
#endif
