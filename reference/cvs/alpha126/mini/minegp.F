      SUBROUTINE MINEGP
C
CKEY MDST /USER
C---------------------------------------------------------------
C! Fill EGPC bank from DFOT.
C
C     Author: Stephen Haywood      04-Nov-91
C---------------------------------------------------------------
C
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "egpcjj.h"
#include "dfotjj.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up DFOT bank.
C
      KDFOT = NLINK('DFOT',0)
      IF(KDFOT.LE.0) RETURN
      NEGPC = LROWS(KDFOT)
      IF(NEGPC.LE.0) RETURN
C
C++   Create EGPC bank.
C
      LEN = LMHLEN + LEGPCA * NEGPC
      CALL AUBOS('EGPC',0,LEN, KEGPC,IGARB)
      CALL BLIST(IW,'S+','EGPC')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDFOT = NLINK('DFOT',0)
      ENDIF
      IW(KEGPC+LMHCOL) = LEGPCA
      IW(KEGPC+LMHROW) = NEGPC
C
C++   Fill EGPC bank.
C
      DO 100 I=1,NEGPC
         RW(KROW(KEGPC,I)+JEGPPX) = FLOAT(ITABL(KDFOT,I,JDFOPX))/EFACTM
         RW(KROW(KEGPC,I)+JEGPPY) = FLOAT(ITABL(KDFOT,I,JDFOPY))/EFACTM
         RW(KROW(KEGPC,I)+JEGPPZ) = FLOAT(ITABL(KDFOT,I,JDFOPZ))/EFACTM
         IQUAL = 0
         IFLAG = ITABL(KDFOT,I,JDFOQF)
         IF(MOD(IFLAG,2).EQ.1) IQUAL = 1
         IF(IFLAG.GT.1) IQUAL = IQUAL + 1110
         IW(KROW(KEGPC,I)+JEGPQU) = IQUAL
         IW(KROW(KEGPC,I)+JEGPPE) = ITABL(KDFOT,I,JDFODE)
  100 CONTINUE
C
      RETURN
      END
#endif
