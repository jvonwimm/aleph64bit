      SUBROUTINE QUPHDP
C----------------------------------------------------------------
C! Fill part of /CUPHCO/
C----------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qhac.h"
#include "cuphco.h"
      PARAMETER( NPPMX= 200 )
      DIMENSION NPLPP(NPPMX),N10PP(NPPMX)
#include "qmacro.h"
C----------------------------------------------------------------
      IPPOB= IW(NAMIND('PPOB'))
      IF(IPPOB.EQ.0) RETURN
      NPPOB= LROWS(IPPOB)
      IF(NPPOB.GT.NPPMX) RETURN
      IPHCO= IW(NAMIND('PHCO'))
      IF(IPHCO.EQ.0) RETURN
      NHCO= LROWS(IPHCO)
C
      IPCRL= IW(NAMIND('PCRL'))
      IF(IPCRL.EQ.0) RETURN
      NPCRL= LROWS(IPCRL)
      CALL QUDPPL(NPLPP,N10PP,IER)
      IF(IER.NE.0) RETURN
C
      DO 10 L=1,NPCRL
        IHCO= ITABL(IPCRL,L,JPCRPH)
        IF(IHCO.LE.0.OR.IHCO.GT.NHCO) GOTO 10
        IPOB= ITABL(IPCRL,L,JPCRPP)
        IF(IPOB.LE.0.OR.IPOB.GT.NPPOB) GOTO 10
        IF(IPOB.EQ.IDPPH(IHCO))        GOTO 10
        EDPPH(IHCO)= EDPPH(IHCO)+RTABL(IPPOB,IPOB,JPPODE)
        IF(NPLPP(IPOB).GT.NPLPH(IHCO)) THEN
          IDPPH(IHCO)= IPOB
          NPLPH(IHCO)= NPLPP(IPOB)
          N10PH(IHCO)= N10PP(IPOB)
        END IF
10    CONTINUE
C
      RETURN
      END
#endif
