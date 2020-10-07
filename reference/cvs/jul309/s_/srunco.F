      SUBROUTINE SRUNCO(THETA,PHI,THUNC,PHUNC)
C----------------------------------------------------------------------
C! Compute track params not corrected for misalignment
C!
C!    Author:     H. Meinhard       05-Oct-1989
C!
C!    Input:      - THETA     /F    corrected track theta
C!                - PHI       /F    corrected track phi
C!    Output:     - THUNC     /F    uncorrected track theta
C!                - PHUNC     /F    uncorrected track phi
C!
C!    Description
C!    ===========
C!    Given theta and phi for a track that was already corrected for
C!    detector misalignment, calculate the uncorrected values in order
C!    to compare them with (uncorrected) coordinates.
C----------------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "saligc.h"
#include "sgeomc.h"
      DOUBLE PRECISION A(3,3),B(3)
      REAL DUMMY(3)
C----------------------------------------------------------------------
C mean value of phi start angle of layers
      PHIB = (PHIBSG(1)+PHIBSG(2)+PHIBSG(3))/3.
C
C get side and module number
      ISIDE = INT(THETA/PIBY2) + 1
      IMOD = 1
      IF (MOD(PHI+TWOPI-PHIB,TWOPI) .GT. PI) IMOD = 2
      IF (ISIDE .EQ. 2) IMOD = IMOD+2
C
C compute impact point of track through fifth module layer
      ZPRIM = -ZOFFSG - REAL(MLAYSG-1)*ZDELSG/2.
      IF (ISIDE .EQ. 2) ZPRIM = -ZPRIM
      B(1) = DBLE(XSHISA(IMOD))
      B(2) = DBLE(YSHISA(IMOD))
      B(3) = DBLE(ZSHISA(IMOD)+ZPRIM)
      A(1,1) = DBLE(SIN(THETA)*COS(PHI))
      A(2,1) = DBLE(SIN(THETA)*SIN(PHI))
      A(3,1) = DBLE(COS(THETA))
      A(1,2) = DBLE(1.)
      A(2,2) = DBLE(XYANSA(IMOD))
      A(3,2) = -DBLE(ZXANSA(IMOD))
      A(1,3) = -DBLE(XYANSA(IMOD))
      A(2,3) = DBLE(1.)
      A(3,3) = DBLE(YZANSA(IMOD))
      CALL DEQN(3,A,3,DUMMY,IER,1,B)
      X = REAL(B(1))*SIN(THETA)*COS(PHI) - XSHISA(IMOD)
      Y = REAL(B(1))*SIN(THETA)*SIN(PHI) - YSHISA(IMOD)
C
C reverse alignment correction
      XPRIM = X + XYANSA(IMOD)*Y - ZXANSA(IMOD)*ZPRIM
      YPRIM = Y + YZANSA(IMOD)*ZPRIM - XYANSA(IMOD)*X
C
C calculate old theta, phi
      ZOFF = ZZERSG
      IF (ISIDE .EQ. 2) ZOFF = -ZZERSG
      Z = ZOFF + ZPRIM
      THUNC = ATAN(SQRT(XPRIM**2+YPRIM**2)/Z)
      IF (THUNC .LT. 0.) THUNC = THUNC + PI
      PHUNC = ATAN2(YPRIM,XPRIM)
      IF (PHUNC .LT. 0.) PHUNC = PHUNC + TWOPI
C
      END
#endif
