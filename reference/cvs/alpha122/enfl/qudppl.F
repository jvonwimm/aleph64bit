      SUBROUTINE QUDPPL(NPLPP,N10PP,IER)
C----------------------------------------------------------------
C! Number of planes of PPOB IDP
C----------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qhac.h"
      PARAMETER( NPPMX= 200 )
      DIMENSION NPLPP(NPPMX),N10PP(NPPMX)
#include "qmacro.h"
C----------------------------------------------------------------
      IER= 0
C
      IPPOB= IW(NAMIND('PPOB'))
      IF(IPPOB.EQ.0) RETURN
      NPPOB= LROWS(IPPOB)
      IF(NPPOB.GT.NPPMX) RETURN
      DO 5 L1=1,NPPOB
        NPLPP(L1)= 0
        N10PP(L1)= 0
5     CONTINUE
C
      IPPDS= IW(NAMIND('PPDS'))
      IF(IPPDS.EQ.0) THEN
        IER= 1
        RETURN
      END IF
      NPPDS= LROWS(IPPDS)
      DO 10 L1=1,NPPDS
        IPP= ITABL(IPPDS,L1,JPPDPP)
        IF(IPP.EQ.0.OR.IPP.GT.NPPOB) GOTO 10
        NPLPP(IPP)= NPLPP(IPP)+1
        INL= ITABL(IPPDS,L1,JPPDNL)
        IDI= ITABL(IPPOB,L1,JPPODI)
        IF(IDI.EQ.-1) GOTO 10
        IF(IDI.EQ.1.AND.INL.GT.13) N10PP(IPP)= N10PP(IPP)+1
        IF(IDI.GT.1.AND.INL.GT.12) N10PP(IPP)= N10PP(IPP)+1
10    CONTINUE
C
      RETURN
      END
#endif
