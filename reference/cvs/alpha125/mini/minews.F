      SUBROUTINE MINEWS
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Ecal wire energy sums bank DWES for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    30-Apr-97
C
C     Input  : PEWI bank
C     Output : DWES bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "pewijj.h"
#include "dwesjj.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up PEWI bank.
C
      KPEWI = NLINK('PEWI',0)
      IF(KPEWI.EQ.0) KPEWI = NLINK('PWEI',0)
      IF(KPEWI.LE.0) RETURN
      NPEWI = LROWS(KPEWI)
      IF(NPEWI.LE.0) RETURN
C
C++   Create DWES bank.
C
      NDWES = NPEWI
      LEN = LMHLEN + LDWESA * NDWES
      CALL AUBOS('DWES',0,LEN, KDWES,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINEWS: Cannot create DWES bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPEWI = NLINK('PEWI',0)
         IF(KPEWI.EQ.0) KPEWI = NLINK('PWEI',0)
      ENDIF
      IW(KDWES+LMHCOL) = LDWESA
      IW(KDWES+LMHROW) = NDWES
C
C++   Loop over PEWI, sum energies on wires and storing odd
C++   and even sums in DWES.
C
      DO 100 I=1,NPEWI
         IW(KROW(KDWES,I)+JDWEMN) = ITABL(KPEWI,I,JPEWMN)
         EPLEVE = 0.
         EPLODD = 0.
         DO IPLAN=1,45
           ENEPL   =  FLOAT(ITABL(KPEWI,I,JPEWPD-1+IPLAN))
           IF(MOD(IPLAN,2).EQ.0) THEN
             EPLEVE = EPLEVE + ENEPL
           ELSE
             EPLODD = EPLODD + ENEPL
           ENDIF
         ENDDO
         EGEVE =  EPLEVE / 1000000.
         EGODD =  EPLODD / 1000000.
         IW(KROW(KDWES,I)+JDWESO) = NINT(EFACTM * EGODD)
         IW(KROW(KDWES,I)+JDWESE) = NINT(EFACTM * EGEVE)
c--         
         EKEV1 = 0.
         EKEV2 = 0.
         EKEV3 = 0.
         EKEV = 0.
         EGEV = 0.
         DO 110 IPLAN=1,10
  110    EKEV1 = EKEV1 + FLOAT(ITABL(KPEWI,I,JPEWPD-1+IPLAN))
         DO 120 IPLAN=11,33
  120    EKEV2 = EKEV2 + FLOAT(ITABL(KPEWI,I,JPEWPD-1+IPLAN))
         DO 130 IPLAN=34,45
  130    EKEV3 = EKEV3 + FLOAT(ITABL(KPEWI,I,JPEWPD-1+IPLAN))
         EKEV = EKEV1 + EKEV2 + EKEV3
         EGEV = EKEV / 1000000.
         IW(KROW(KDWES,I)+JDWEEF+0) = NINT(1000. * EKEV1/EKEV )
         IW(KROW(KDWES,I)+JDWEEF+1) = NINT(1000. * EKEV2/EKEV )
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DWES')
C
      RETURN
      END
#endif
