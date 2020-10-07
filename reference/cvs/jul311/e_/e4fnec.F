      SUBROUTINE E4FNEC
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR : STOP EXTRAPOLATION FOR ENDCAPS
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
#include "e4com0.h"
#include "e4par1.h"
#include "e4com1.h"
#include "rflags.h"
#include "rparac.h"
#include "rlunit.h"
      IF(SQRT(XI**2+YI**2).GT.RLIMEC)THEN
C
      IF( A(1).EQ.0 )THEN
        YI=XTOW(ITRCL,1,2)
        GOTO 1
      ENDIF
      G=1./A(1)
      H=XTOW(ITRCL,1,1)-G*XTOW(ITRCL,1,2)
      DDE=G**2+1.
      TPETIT=DDE*RLIMEC**2-H**2
      IF(TPETIT.LT.0)TPETIT=0
      YI1=(-G*H+SQRT(TPETIT))/DDE
      YI2=(-G*H-SQRT(TPETIT))/DDE
      IF(ABS(YI-YI1).GT.ABS(YI-YI2))THEN
             YI=YI2
             ELSE
             YI=YI1
             ENDIF
C
 1    CONTINUE
      UPETIT=RLIMEC**2-YI**2
      IF(UPETIT.LT.0)UPETIT=0
      XI1=SQRT(UPETIT)

      XI2=-XI1
      IF(ABS(XI-XI1).GT.ABS(XI-XI2))THEN
             XI=XI2
             ELSE
             XI=XI1
             ENDIF
      ZI=A(2)*(XI-XTOW(ITRCL,1,1))+XTOW(ITRCL,1,3)
      IF(JDBDRF(JULEC).GT.8)
     &WRITE(LDEBRL,*)'EXIT TOP OF END CAP XI YI ZI',XI,YI,ZI
      INDI=4
      ENDIF
      RETURN
      END
#endif
