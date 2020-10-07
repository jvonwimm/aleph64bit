      SUBROUTINE QUPHPL(ICL,NPL,N10,NHT)
C----------------------------------------------------------------
C!  Scan HTUB bank looking for the number of planes (total and
C!  in the last 10 ) and for the number of hits associated to
C!  a given PHCO.
C
C   Gerardo Ganis -- 1989
C----------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qhac.h"
      DIMENSION IMOD(2),IPLA(23),NPLA(23),IMFI(2)
#include "qmacro.h"
C----------------------------------------------------------------
      DO L1=1,23
        IF(L1.LE.2) IMFI(L1)= 0
        IPLA(L1)= 0
        NPLA(L1)= 0
      END DO
C----- Get module & region number
      CALL QUHCRG(ICL,IREG,IMOD)
      ISC0= 1
      IMD0= IMOD(1)
      IF(IREG.EQ.3) THEN
        ISC0= 2
        IMD0= IMOD(2)
      END IF
      IF(IREG.GE.4) ISC0= 3
C
        NSC= 0
5     CONTINUE
      NPL= 0
        NSC= NSC+1
      IHTUB= IW(NAMIND('HTUB'))
      IF(IHTUB.EQ.0) RETURN
      NHTUB= LROWS(IHTUB)
      DO 10 L1=1,NHTUB
        ISCL= ITABL(IHTUB,L1,JHTUSN)
        IMDL= ITABL(IHTUB,L1,JHTUMN)
        IF(ISCL.NE.ISC0.OR.IMDL.NE.IMD0) GOTO 10
        IMFI(NSC)= 1
        ILNL= ITABL(IHTUB,L1,JHTULN)
        IPLA(ILNL)= 1
        NFIR= INT(RTABL(IHTUB,L1,JHTUCW))
        NPLA(ILNL)= NPLA(ILNL)+ NFIR
10    CONTINUE
      IF((IREG.EQ.2.AND.ISC0.EQ.1).OR.
     &   (IREG.EQ.4.AND.ISC0.EQ.3))   THEN
        ISC0= 2
        IMD0= IMOD(2)
        GOTO 5
      END IF
C
      IGOO= 1
C- Check if both barrel and endcap tubes are fired for clusters
C- in overlaps
      IF(IREG.EQ.2.OR.IREG.EQ.4) THEN
        IF(IMFI(1).EQ.0.OR.IMFI(2).EQ.0) IGOO= 0
      END IF
C
      NPL= 0
      N10= 0
      NHT= 0
      IF(IGOO.EQ.0) RETURN
      DO 20 L1=1,23
        NPL= NPL+IPLA(L1)
        IF(L1.GT.13) N10= N10+IPLA(L1)
        NHT= NHT+NPLA(L1)
20    CONTINUE
C
      RETURN
      END
#endif
