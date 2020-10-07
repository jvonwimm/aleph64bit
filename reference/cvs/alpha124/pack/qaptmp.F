      SUBROUTINE QAPTMP
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! translate old fashioned particle table into the new one.
C  create a new PART bank and a translation table QTMP.
C  delete this routine if old tables do not exist any more.
C  called from QMNEWR
C!                                                  H.Albrecht 12.01.89
C----------------------------------------------------------------------
#ifndef DOC
C
#include "qcde.h"
C       used HAC parameters are the same for old and new table :
#include "partjj.h"
      CHARACTER * 12 CNAME,SPACES
C
      IPART = IW(NAPART)
      LPART = IW(IPART+1)
      NPART = IW(IPART+2)
C
C       temporary bank --> new PART bank
C
      ITEMP = NBANK ('TEMP',0, NPART*KCQPAR+LMHLEN)
      IW(ITEMP+1) = KCQPAR
      IW(ITEMP+2) = NPART
      NKLIN = IW(NAKLIN)
C
C       loop over all particles
C
      IPART = IPART + LMHLEN
      ITEMP = ITEMP + LMHLEN
      DO 80 N=1,NPART
C
C         find row in new table
C
        NNEW = 0
        IF (NKLIN .EQ. 0)  GO TO 10
        IF (N .GT. IW(NKLIN+2))  GO TO 10
        NLUND = IW(NKLIN+N+LMHLEN)
        IF (NLUND .EQ. 0)  GO TO 10
        NNEW = KGLUND (NLUND)
C
C         fill new bank
C
   10   I = IPART
        IF (NNEW .NE. 0)  I = KOQPAR + NNEW * KCQPAR
        IW(ITEMP+JQPANA) = IW(I+JPARNA)
        IW(ITEMP+JQPANA+1) = IW(I+JPARNA+1)
        IW(ITEMP+JQPANA+2) = IW(I+JPARNA+2)
        IW(ITEMP+JQPAGN) = IW(IPART+JPARGN)
        IW(ITEMP+JQPACO) = IW(IPART+JPARCO)
        IW(ITEMP+JQPAMA) = IW(IPART+JPARMA)
        IW(ITEMP+JQPACH) = IW(IPART+JPARCH)
        IW(ITEMP+JQPALT) = IW(IPART+JPARLT)
        RW(ITEMP+JQPAWI) = 0.
        IW(ITEMP+JQPAAN) = N
      ITEMP = ITEMP + KCQPAR
   80 IPART = IPART + LPART
C
C       drop old PART bank and rename TEMP bank
C
      N = NDROP ('PART',0)
      CALL BSWAP (IW,'TEMP','PART')
C
   90 END
#endif
