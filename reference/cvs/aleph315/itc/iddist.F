      REAL FUNCTION IDDIST(IL,TIME,DDDT)
C-----------------------------------------------------------------------
CKEY ITC
C! Drift-distance calc. from drift-time relation.
C!
C!    Author    :- J.Sedgbeer 93/07/01
C!    Modified  :- J.Sedgbeer 95/06/10 Protect for v.large times beyond
C!                 reasonable input range of t-d relation.
C!    Modified  :- J.Sedgbeer 98/08/16 Modify to use polynom. or spline
C!                            drift-time coeffs. depending on the flag 
C!                            IFLGID in /IDRPCC/.
C!                            Note that only the array of coeffs (in 
C!                            /IDRPCC/) corresponding to IFLGID will
C!                            be set - the other array is undefined.
C!    Input:
C!      IL     /I  : ITC layer number
C!      TIME   /I  : Drift time - fully corrected (ns)
C!      need commons     /IDRPCC/ Drift-time relation.
C!                       /ITWICC/ ITC geom.
C!
C!    Output:
C!     IDDIST  /R  : Drift-distance (cm.). Max value = cell width.
C!     DDDT    /R  : dD/dt - gradient of drift-relation at time TIME
C!                      - for poly. set negative if v. large input time.
C!                      - for spline set =  0.0  if v. large input time. 
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
      REAL TN,FSP
      real T1,T2,A1,A2,A3,B2
C-----------------------------------------------------------------------
C
      DIST = 0.0
      DDDT = 0.0
C 
C-----------------------------------------------------------------------
C Polynomial drift-time parametrisation (for data up to 1996 and all MC)
C
      IF (IFLGID.EQ.0) THEN
C
        DO 10 I=MCOFID,1,-1
          DDDT = DDDT*TIME + FLOAT(I)*DTCOID(I,IL)
          DIST = (DIST + DTCOID(I,IL)) * TIME
   10   CONTINUE
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
C-----------------------------------------------------------------------
C Spline drift-time  parametrisation (for data from 1997 onwards)
C
      ELSE
C
        T1 = DSPLID(1,IL)
        T2 = DSPLID(2,IL)
        A1 = DSPLID(3,IL)
        A2 = DSPLID(4,IL)
        A3 = DSPLID(5,IL)
        B2 = 3.0 * (1.0 - (5.0*A1/3.0 + 7.0*A2/3.0 + 3.0*A3) )
C
        FSP  = 0.0
C
        IF (TIME.GT.0.0 .AND. TIME.LE.T1) THEN
          TN  = TIME/T1
          FSP = A1*TN + A2*TN**2 + A3*TN**3
          DDDT = A1 + 2.0*A2*TN + 3.0*A3*TN**2
          DDDT = DDDT * 0.5*CELWIT(IL) /T1
C
        ELSEIF (TIME.GT.T1 .AND. TIME.LE.T2) THEN
          TN  = (TIME-T1)/(T2-T1)
          FSP = (A1+A2+A3) + (A1 + 2.0*A2 + 3.0*A3)*TN + B2*TN**2 
     +                     - (A1 + 2.0*A2 + 3.0*A3 + 2.0*B2)*TN**3/3.
          DDDT =           + (A1 + 2.0*A2 + 3.0*A3)    + 2.0*B2*TN
     +                     - (A1 + 2.0*A2 + 3.0*A3 + 2.0*B2)*TN**2
          DDDT = DDDT * 0.5*CELWIT(IL) / (T2-T1)
C          
        ELSEIF (TIME.GT.T2) THEN
          FSP  = 1.0
          DDDT = 0.0
        ENDIF
C      
        DIST = FSP * 0.5*CELWIT(IL)

      ENDIF
C
      IDDIST = DIST
C
      END
#endif
