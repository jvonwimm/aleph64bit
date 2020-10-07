      SUBROUTINE YVHIST
C----------------------------------------------------------------
C! V0 histograms
C!
C!    Author: M.A. Ciocci,L. Rolandi 4/5/88
C!    Modified: M.Bosman 12/04/89
C!    Modified: M.A.Ciocci 16/4/89
C!    Modified: M.A.Ciocci 6/6/90
C----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rflags.h"
#include "rparac.h"
#include "yv0vjj.h"
#include "bosext.h"
C
         REAL PV0(3),PXM(2,2),PX(2)
#include "bmacro.h"
C
C filling of V0 histograms is done for histograming level 2 minimum
         IF(JHISRF(JULYR).LT.2.AND.JHISRF(JULYT).LT.2)GO TO 999
C the offset is defined with JULYR or JULYT
C in case both packages are called , JULYT is used
         IF(FDETRF(JULYR)) JOFF = JULYR * 1000
         IF(FDETRF(JULYT)) JOFF = JULYT * 1000
C
         KYV0V=IW(NAMIND('YV0V'))
         IF(KYV0V.EQ.0)GO TO 999
         NV0=LROWS(KYV0V)
         IF(NV0.EQ.0) GO TO 999
         DO 20 IV0=1,NV0
         CHI2V=RTABL(KYV0V,IV0,JYV0C2)
             IF(ABS(ITABL(KYV0V,IV0,JYV0IC)).LT.20.)THEN
         CALL HF1(JOFF+100,CHI2V,1.)
             ELSE
         CALL HF1(JOFF+101,CHI2V,1.)
             ENDIF
         PX(1)=RTABL(KYV0V,IV0,JYV0X1)
         PX(2)=RTABL(KYV0V,IV0,JYV0X2)
         PV0(1)=RTABL(KYV0V,IV0,JYV0PX)
         PV0(2)=RTABL(KYV0V,IV0,JYV0PY)
         PV0(3)=RTABL(KYV0V,IV0,JYV0PZ)
        NPOIN=JYV0XM-1
        DO 1 I=1,2
        DO 2 J=1,I
        NPOIN=NPOIN+1
        PXM(I,J)=RTABL(KYV0V,IV0,NPOIN)
        PXM(J,I)=PXM(I,J)
  2     CONTINUE
  1     CONTINUE
        CALL YFQERQ('K0',PX,PXM,PV0,QVAL,ERQ2)
        CALL HF1(JOFF+102,QVAL,1.)
        CALL YFQERQ('LA',PX,PXM,PV0,QVAL,ERQ2)
        CALL HF1(JOFF+103,QVAL,1.)
        CALL YFQERQ('AL',PX,PXM,PV0,QVAL,ERQ2)
        CALL HF1(JOFF+103,QVAL,1.)
        CALL YFQERQ('GA',PX,PXM,PV0,QVAL,ERQ2)
        CALL HF1(JOFF+104,QVAL,1.)
  20    CONTINUE
  999   CONTINUE
      RETURN
      END
#endif
