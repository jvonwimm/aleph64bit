      SUBROUTINE ECENER(IT,ICLN)
C---------------------------------------------------------------------
C!  ENERGY PER STACK IN THE 2X2 STOREYS CLOSE TO THE CHARGED TRACK
C!
C!  AUTHOR : A. FALVARD
C!  MODIFIED :
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "e4com1.h"
#include "ecmode.h"
#include "etpinf.h"
      PARAMETER (ELIMIT=5.)
      CALL VZERO(ESTACK,LESTAC)
      NECLU=NLINK('ECLU',0)
      IPNCLU=NECLU+2+(ICLN-1)*IW(NECLU+1)
      ENERCL=RW(IPNCLU+2)
      IF(ENERCL.GT.ELIMIT)THEN
        DO 1 I=1,NBE4ST(IT)
        K=KSTO(IT,I)
        ESTACK(K)=ESTACK(K)+ESTO(IT,I)
  1     CONTINUE
        ELSE
        NESDA=NLINK('ESDA',0)
        DO 2 I=1,IW(NESDA+2)
      IPOIN=NESDA+2+(I-1)*IW(NESDA+1)
        NCLU=IW(IPOIN+9)
        IF(NCLU.NE.ICLN)GOTO 2
        IK=IW(IPOIN+3)
        ESTACK(IK)=ESTACK(IK)+RW(IPOIN+4)
 2      CONTINUE
        ENDIF
      RETURN
      END
#endif
