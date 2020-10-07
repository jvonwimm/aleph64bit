      SUBROUTINE E4TSTL(IGOT)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR : CHECK IF EXTRAPOLATION OUT OF ECAL
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
#include "e4com0.h"
#include "rlunit.h"
#include "rflags.h"
#include "rparac.h"
#include "e4par1.h"
C
      PARAMETER (EPSILO=0.001)
C
      DIMENSION DDX(3)
      INTEGER EFNDMD,EFNDSC
      EXTERNAL EFNDMD,EFNDSC,FRACST
C
      DDX(1)=XI
      DDX(2)=YI
      DDX(3)=ZI
      ISUB1=EFNDSC(DDX)
      IMOD1=EFNDMD(ISUB,DDX)
      IF(JDBDRF(JULEC).GT.8)
     &WRITE(LDEBRL,*)'ISUB1,IMOD1',ISUB1,IMOD1
C
      IF(ISUB1.EQ.ISUB)THEN
             IF(IMOD1.EQ.IMOD)THEN
                       ZLIMO1=ZLIMOD+EPSILO
                       IF(ISUB.EQ.2.AND.ABS(ZI).GT.ZLIMO1)THEN
C                        WE ARE OUT OF SENSITIVE ZONE OF BARREL
C                        WE STOP AT Z=ZLIMMOD
C                        SO WE EXIT BY  OVERLAP SIDE.
C                        CORR 20/07/90 : WHEN FIRST PLAN OUTSIDE BR
                         IGOT=1
                         IF(INDI.EQ.2)IGOT=3
C     IF(IGOT.EQ.3)PRINT*,'IGOT=3'
                         INDI=3
                         ISUB2=3
                         IF(ZI.GT.0)ISUB2=1
                         RETURN
                         ENDIF
C                      ALL IS OK WE CAN GO THROUGH NEXT PLAN
                       IF(INDI.NE.3)INDI=MOD(INDI,2)+1
                       IGOT=2
             ELSE
C            WE ARE GOING THROUGH AN OTHER MODULE WE START AGAIN IMOD1
                       IMOD=IMOD1
                       IGOT=1
             ENDIF
      ELSE
             IF(INDI.EQ.1)THEN
C WE EXIT FROM BARREL THROUGH EDGE SIDE. WE STOP AT Z=ZLIMMOD
                       INDI=3
                       IGOT=1
                       ISUB2=ISUB1
                       RETURN
             ENDIF
             IF(INDI.EQ.2)THEN
C WE ARE NOW IN ENDCAP! FOR THE FIRST POINT OF THE VOLUME
                       ISUB=ISUB1
                       KSINT=1
                       SK=FLOAT(KSINT)+FRACST(KSINT,INDI)
                       IGOT=1
                       RETURN
             ENDIF
             IF(INDI.EQ.3)THEN
C     INDI=3             IMPOSSIBLE BECAUSE WE'VE ALREADY CHANGE=>ERROR
                       WRITE(LDEBRL,*)'EIDENT> INTERSECTION ERROR'
                       INDI=4
                       IGOT=2
                       RETURN
             ENDIF
      ENDIF
      RETURN
      END
#endif
