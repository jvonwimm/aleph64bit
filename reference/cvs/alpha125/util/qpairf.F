      SUBROUTINE QPAIRF(I1,I2,DXY,DZ0,DZ2,DTH,RMA,ZMA,XMA,NC1,
     &                  DIN1,NC2,DIN2,P,IER)
CKEY KINEMATICS /USER
C----------------------------------------------------------------------
C   Author   : E. Blucher            22-APR-1989
C   Modified : G. Graefe             22-APR-1994 input from NanoDst
C   Description
C   ===========
C! Calls Dave Cinabro's PAIRFD routine.
C
C Inputs: I1 = Alpha track number (in CHT section)
C         I2 = Another ALPHA track (in CHT section)
C Output: DXY = the distance(cm) in the xy plane at the closest approach
C         DZ2 = the distance(cm) between the tracks in z at the xy point
C         DZ0 = the distance(cm) between the tracks in z at the origin
C         DTH = the theta difference of the two tracks
C         RMA = the rho value at the average materialization point
C         ZMA = the z value at the average materialization point
C         XMA = the invariant mass of the tracks at the materializtion
C               point assuming they are both electrons
C         NC1(2) = Number of coordinates with radius inside RMA for t
C                  track 1(2).  0 if no coordinate information available
C         DIN1(2) = Distance(cm) in radius that the furthest coordinate
C                   is inside of RMA for track 1(2).  0. if no coordinat
C                   information is available.
C         P(3) = Summed momentum of the two tracks at the materializatio
C                point
C         IER = 0 if all OK
C               1 if error in calculation
C======================================================================
#ifndef DOC
#include "qcde.h"
      SAVE IPRER
      DATA IPRER / 0 /
      DIMENSION P(*)
#include "qmacro.h"
C----------------------------------------------------------------------
      IT1=KCHT(I1)
      IT2=KCHT(I2)
      IF (XNANO) THEN
        CALL NAIRFD(IT1,IT2,DXY,DZ0,DZ2,DTH,RMA,ZMA,
     &              XMA,NC1,DIN1,NC2,DIN2,P,IER)
      ELSE
        CALL PAIRFD(KTN(IT1),KTN(IT2),QMFLD,DXY,DZ0,DZ2,DTH,RMA,ZMA,
     &              XMA,NC1,DIN1,NC2,DIN2,P,IER)
      ENDIF
      IF (IER.NE.0) THEN
         IPRER = IPRER + 1
         IF (IPRER.LE.10) CALL QWMESE('_QPAIRF_ Error in PAIRFD ')
      ENDIF
      END
#endif
