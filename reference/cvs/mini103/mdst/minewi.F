      SUBROUTINE MINEWI
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Ecal wire energy bank DEWI for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PEWI bank
C     Output : DEWI bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "pewijj.h"
#include "dewijj.h"
#include "bmacro.h"
C
C++   Pick up PEWI bank.
C
      KPEWI = NLINK('PEWI',0)
      IF(KPEWI.EQ.0) KPEWI = NLINK('PWEI',0)
      IF(KPEWI.LE.0) RETURN
      NPEWI = LROWS(KPEWI)
      IF(NPEWI.LE.0) RETURN
C
C++   Create DEWI bank.
C
      NDEWI = NPEWI
      LEN = LMHLEN + LDEWIA * NDEWI
      CALL AUBOS('DEWI',0,LEN, KDEWI,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINEWI: Cannot create DEWI bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPEWI = NLINK('PEWI',0)
      ENDIF
      IW(KDEWI+LMHCOL) = LDEWIA
      IW(KDEWI+LMHROW) = NDEWI
C
C++   Loop over PEWI, sum energies on wires and storing totals in DEWI.
C
      DO 100 I=1,NPEWI
         IW(KROW(KDEWI,I)+JDEWMN) = ITABL(KPEWI,I,JPEWMN)
         EKEV1 = 0.
         EKEV2 = 0.
         EKEV3 = 0.
         DO 110 IPLAN=1,10
  110    EKEV1 = EKEV1 + FLOAT(ITABL(KPEWI,I,JPEWPD-1+IPLAN))
         DO 120 IPLAN=11,33
  120    EKEV2 = EKEV2 + FLOAT(ITABL(KPEWI,I,JPEWPD-1+IPLAN))
         DO 130 IPLAN=34,45
  130    EKEV3 = EKEV3 + FLOAT(ITABL(KPEWI,I,JPEWPD-1+IPLAN))
         EKEV = EKEV1 + EKEV2 + EKEV3
         EGEV = EKEV / 1000000.
         IW(KROW(KDEWI,I)+JDEWE0) = NINT(EFACTM * EGEV)
         IW(KROW(KDEWI,I)+JDEWEF+0) = NINT(1000. * EKEV1/EKEV )
         IW(KROW(KDEWI,I)+JDEWEF+1) = NINT(1000. * EKEV2/EKEV )
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DEWI')
C
      RETURN
      END
#endif
