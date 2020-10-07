      SUBROUTINE YVSTKE(WTM,VXP,VZP,VXZ)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Finds track error**2 in jet coordinate system
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Argument :
C     WTM(,) IS 3D TRACK WEIGHT MATRIX IN JET SYSTEM
C  Output Arguments :
C  *  VXP IS TRACK VARIANCE (SIGMA**2) IN D0-LIKE DIRECTION
C  *  VZP IS TRACK VARIANCE IN Z0-LIKE DIRECTION
C  *  VXZ IS COVARIANCE
C
C ------------------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      DIMENSION WTM(3,3)
      REAL*8 T1,T2
C ------------------------------------------------------------------------------
C
C ASSUME BIG VARIANCES IN CASE OF FAILURE
      VXP=999.
      VZP=999.
      VXZ=0.
C
C DETERMINANT OF SUBMATRIX
      T1=WTM(1,1)
      T1=T1*WTM(2,2)
      T2=WTM(2,1)
      T2=T2*WTM(1,2)
      DET=T1-T2
      W11=WTM(1,1)
      W22=WTM(2,2)
      COR=SQRT(ABS(W11*W22))
      IF (COR .NE. 0.) COR=WTM(1,2)/COR
C
      IF (DET .LE. 0.) THEN
C TRACK WEIGHT MATRIX IS NONSENSE
CCCC    WRITE (IW(6),19) DET
   19   FORMAT (' ## YVSTKE ## : DET=',1PE10.3)
CCCC    WRITE (IW(6),29) W11,W22,COR
   29   FORMAT (' W11=',1PE10.3,' W22=',1PE10.3,' COR=',0PF10.6)
C GIVE UP
        RETURN
      ELSE
C INVERT THE SUBMATRIX
        CALL RSINV(2,WTM,3,IFAIL)
C CHECK THE RESULT
        IF (IFAIL .NE. 0) THEN
          WRITE (IW(6),39) IFAIL
   39     FORMAT (' ## YVSTKE ## : IFAIL=',I3)
          WRITE (IW(6),29) W11,W22,COR
        ELSE
C COPY THE RESULTS
          VXP=WTM(1,1)
          VZP=WTM(2,2)
          VXZ=WTM(1,2)
        ENDIF
      ENDIF
      RETURN
      END
#endif
