       SUBROUTINE GVERCR(IPECO,RAD,IER)
C======================================================================
C! Calculate cluster coordinate
C! Author   :- MN Minard             27-JAN-1993
C      Input
C-     IPECO / I = PECO row number
C      Output
C-     RAD   / R = Radius of shower
C      IER   / I = error code
C ====================================================================
#ifndef DOC
#include "bcs.h"
#include "pestjj.h"
#include "etdijj.h"
       DIMENSION XPOINT(4)
       DATA NAPEST, NAETDI , NAPYER /3*0/
#include "bmacro.h"
       IF (NAPEST.EQ.0) THEN
         NAPEST = NAMIND('PEST')
         NAETDI = NAMIND('ETDI')
         NAPYER = NAMIND('PYER')
       ENDIF
       KPEST = IW(NAPEST)
       NPEST = 0
       IF (KPEST.NE.0) NPEST = LROWS(KPEST)
       KETDI = IW(NAETDI)
       NETDI = 0
       IF (KETDI.NE.0) NETDI = LROWS(KETDI)
       KPYER = IW(NAPYER)
       NCELL = 0
       E1 = 0.
       E2 = 0.
       E3 = 0.
       ENS = 0.
       DO IPEST = 1, NPEST
         IF ( ITABL(KPEST,IPEST,JPESPE).EQ.IPECO)NCELL=NCELL + 1
         IF ( ITABL(KPEST,IPEST,JPESPE).EQ.IPECO.AND.
     &        ITABL(KPEST,IPEST,JPESET).GT.0)THEN
C
C-       Look if corresponding ETDI has energy
C
           JETDI = ITABL(KPEST,IPEST,JPESET)
           K = ITABL(KPEST,IPEST,JPESKS)
           EN = FLOAT(ITABL(KETDI,JETDI,K+1))/1000000.
           IADDS = ITABL(KETDI,JETDI,1)
           ITET = IBITS(IADDS,16,8)
           IPHI = IBITS(IADDS,2,9)
           IF (ITET.GT.0.AND.ITET.LT.229) THEN
              IF(IPHI.GT.0.AND.IPHI.LT.384) THEN
                CALL ESRBC('ALEPH',ITET,IPHI,K,XPOINT)
                E1 = E1 + EN*XPOINT(1)
                E2 = E2 + EN*XPOINT(2)
                E3 = E3 + EN*XPOINT(3)
                ENS = ENS + EN
              ENDIF
            ENDIF
         ENDIF
       ENDDO
       IER = 0
       IF ( ENS.GT.0) THEN
         EX1 = E1/ENS
         EX2 = E2/ENS
         EX3 = E3/ENS
         RAD = SQRT(EX1**2+EX2**2+EX3**2)
       ELSE
        IER = 1
       ENDIF
       RETURN
       END
#endif
