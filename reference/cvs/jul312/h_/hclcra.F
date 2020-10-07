      SUBROUTINE HCLCRA
C------------------------------------------------------------------
C
C! fill region code in HCLU
C
C!     R.Tenchini            date : 890510
C!     INPUT BANKS: HCLU, HSTO, HSDA
C!     called by HCFCLU
C
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "hcnamc.h"
#include "hclujj.h"
#include "hsdajj.h"
#include "hstojj.h"
      DIMENSION XX(3),PIN(3,2),POU(3,2)
      CHARACTER*16 WRONG
      INTEGER ST0
#include "bmacro.h"
C
C?              LINK HCLU HSTO HSDA
C
      KHCLU=IW(NAHCLU)
      NHCLU=0
      IF(KHCLU.NE.0) THEN
         NHCLU=LROWS(KHCLU)
      ENDIF
      KHSTO=IW(NAHSTO)
      KHSDA=IW(NAHSDA)
      DO 10 I=1,NHCLU
         IMOBN = 0
         IMOEN = 0
         IHSDA=ITABL(KHCLU,I,JHCLFS)
 20      CONTINUE
C
C Loop on storey inner face corners
C
         DO 30 K=0,3
            XX(1) = RTABL(KHSTO,IHSDA,JHSTXI+K)
            XX(2) = RTABL(KHSTO,IHSDA,JHSTYI+K)
            XX(3) = RTABL(KHSTO,IHSDA,JHSTZI+K)
            CALL EMPACT(XX,NSC,PIN,POU)
            DO 40 J=1,NSC
               XX(1) = POU(1,J)
               XX(2) = POU(2,J)
               XX(3) = POU(3,J)
               CALL EFNDTW(XX,'ALEPH',I0,J0,ST0,ISC,MD,KPL,WRONG)
               IF(WRONG.EQ.'SUBCOMPONENT'.OR.WRONG.EQ.'MODULE') THEN
                  CALL RERROR('HCLCRA',1,' WARNING FROM ECALDES')
               ELSE
                  IF(ISC.EQ.2) THEN
                     IF(IMOBN.EQ.0) THEN
                        IMOBN = MD
                        GO TO 40
                     ELSE
                        IF(IMOBN.NE.MD) GO TO 444
                     ENDIF
                  ELSE
                     IF(IMOEN.EQ.0) THEN
                        IMOEN = MD
                        GO TO 40
                     ELSE
                        IF(IMOEN.NE.MD) GO TO 444
                     ENDIF
                  ENDIF
               ENDIF
 40         CONTINUE
 30      CONTINUE
         INEXT=ITABL(KHSDA,IHSDA,JHSDNS)
         IF(INEXT.NE.0) THEN
            IHSDA=INEXT
            GO TO 20
         ENDIF
         GO TO 10
 444     CONTINUE
         IW(KROW(KHCLU,I)+JHCLKD)=IW(KROW(KHCLU,I)+JHCLKD)+2
 10   CONTINUE
      RETURN
      END
#endif
