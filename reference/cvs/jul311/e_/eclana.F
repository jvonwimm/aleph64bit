      SUBROUTINE ECLANA(KCL,KCT)
C----------------------------------------------------------------------
C!  - ROUTINE FOR ELECTRON IDENTIFICATION BUILD EIGA BANK AND FILL EIDT
C!
C!  AUTHOR   : D. PALLIN
C!  MODIFIED :
C!
C!  BANKS :
C!    INPUT   : NONE
C!    OUTPUT  : EIGA
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "eclujj.h"
#include "eidtjj.h"
#include "ecnamc.h"
#include "eigajj.h"
#include "e4par1.h"
#include "eibpr2.h"
      DATA FIRST/0/
#include "bmacro.h"
C
C    KCT = NBER OF TRACKS BELONGING TO CLUSTER KCL
C
      IER=0
      KECLU=IW(NAECLU)
      NECLU=LROWS(KECLU)
      KEIDT=IW(NAEIDT)
      NEIDT=LROWS(KEIDT)
      DO 1 I=1,NEIDT
      KLINE=KROW(KEIDT,I)
      JCL=IW(KLINE+JEIDPE)
C
C     FOR THIS CLUSTER KCL,WE FILL ECORR FOR IDENTIFIED ELECTRON ONLY
C
      IF(JCL.EQ.KCL)THEN
         IPP=IW(KLINE+JEIDIP)
         IF(IPP.EQ.1)THEN
C        IPP=1 ELECTRON IDENTIFIED
C        ECORR=ECLU+ECLOSS+(E4/.83)*SATURATION
C        ECLOSS= CORRECTION FOR LEAKAGE AND CLUSTERISATION
            CALL ECLOST(JCL,ECLOSS,IER)
            ENOR=(RW(KLINE+JEIDE1)+RW(KLINE+JEIDE2)+
     &      RW(KLINE+JEIDE3))/XE4000
            FAC = 1.-4.*SATURE*ENOR
            ESATUR = 0.
            IF ( FAC.GT.0.00001) THEN
              ESATUR = (1.-SQRT(FAC))/(2.*SATURE)
              ESATUR = ESATUR-ENOR
            ENDIF
            RW(KLINE+JEIDEC)=RW(KROW(KECLU,KCL)+JECLE4)+ECLOSS
     &      +ESATUR
         ENDIF
      ENDIF
C
C
C
 1    CONTINUE
      RETURN
      END
#endif
