      SUBROUTINE OMIPOS (IDPA, XYBOM, XIP, IRET)
C----------------------------------------------------------------------
C!  - Calculate beam position and angle at the IP using the BOM position
C!
C!   Author:  R.W.Forty  23-May-91
C!            based on code from W.Kozanecki
C!
C!   Input:  XYBOM = 4-component vector of x and y positions
C!                   read by BOMs: (xA, xB, yA, yB)
C!           IDPA  = 1  e+
C!                   2  e-
C!   Output: XIP   = 4-component vector containing the positions
C!                   and angles at the IP: (x, x', y, y')
C!           IRET  = 0  OK
C!                >< 0  Error
C?
C!======================================================================
#ifndef DOC
#include "bomopt.h"
#include "bomcal.h"
C
      INTEGER IDPA, KA, KB, KC, I, IBA, KCBA, IXY, IFAIL, IRET
      REAL XYBOM(*), XIP(*)
C
C  XCV Corrector kick vector of the form (0,x' ,0,0) or (0,0,0,y')
C      depending of whether it is an X- or a Y- corrector
C  XC  Corrector kick (x' or y'   as defined for XCV)
C  VA,VB,RW4  work area vectors and matrix
C  DV  D vector
C
      REAL XCV(4), XC, VA(4), VB(4), RW4(4,4), DV(4)
C.......................................................................
C
C  Inverse beam transport equations
C
      IF (IDPA .EQ. 1) THEN
        KA = 2
        KB = 1
      ELSE
        KA = 1
        KB = 2
      ENDIF
C
      CALL VZERO (DV,4)
C
      DO 300 IBA = 1, 2
C
C  IBA = 1 refers to top half of D vector, IBA = 2 to bottom half
C
        DO 200 KC = 1, NUMCOR
          CALL VZERO (XCV,4)
          IF (IBA. EQ. 1) THEN
            KCBA = KB
          ELSE
            KCBA = KA
          ENDIF
C
          XC = DEFCOR(KC,KCBA)
          IXY = IXYCOR(KC)
C
C  Flip sign of corrector deflections for horizontal corrector on e+
C
          IF (IDPA .EQ. 1 .AND. IXY .EQ. 1) XC = -XC
          XCV(2*IXY) = XC
          IF (IBA .EQ. 1) THEN
            CALL RMMLT (4, 4, 1, AMCIOP(1,1,KB,KC), AMCIOP(1,2,KB,KC),
     &                  AMCIOP(2,1,KB,KC), XCV(1), XCV(1), XCV(2),
     &                  VA(1), VA(1), VA(2), VB)
          ELSE
            CALL RMMLT (4, 4, 1, RCBOPT(1,1,KA,KC), RCBOPT(1,2,KA,KC),
     &                  RCBOPT(2,1,KA,KC), XCV(1), XCV(1), XCV(2),
     &                  VA(1), VA(1), VA(2), VB)
            VA(1) = -VA(1)
            VA(3) = -VA(3)
          ENDIF
C
          DO 100 I = 1, 2
            DV((IBA-1)*2+I) = DV((IBA-1)*2+I) + VA(2*I-1)
  100     CONTINUE
  200   CONTINUE
  300 CONTINUE
C
C  Add measured BOM positions
C
      VA(1) = XYBOM(KB)
      VA(2) = XYBOM(KB+2)
      VA(3) = XYBOM(KA)
      VA(4) = XYBOM(KA+2)
C
C  VA  vector of input BOM data, in the order (if analyzing e-):
C      (x at BOM B, yB, xA, yA).  For positrons, remember x+ = - x-
C
      IF (IDPA .EQ. 1) THEN
        DO 400 I = 1, 3, 2
          VA(I) = -VA(I)
  400   CONTINUE
      ENDIF
C
      DO 500 I = 1, 4
        DV(I) = DV(I) + VA(I)
  500 CONTINUE
C
C  Ready to solve the linear system ZMOPTI * XIP = DV  for XIP
C
      CALL UCOPY (ZMOPTI(1,1,IDPA), RW4, 16)
      CALL REQN (4, RW4, 4, VA, IFAIL, 1, DV)
      IF (IFAIL .EQ. 0) THEN
        CALL UCOPY (DV, XIP, 4)
      ELSE
        GOTO 998
      ENDIF
C
C  If positrons, convert to e- coordinate system
C
      IF (IDPA .EQ. 1) THEN
        XIP(1) = -XIP(1)
        XIP(4) = -XIP(4)
      ENDIF
C
      IRET = 0
      GOTO 999
C
  998 CALL RERROR ('OMIPOS', 1, ' Error calculating position at IP')
      IRET = 1
C
  999 RETURN
      END
#endif
