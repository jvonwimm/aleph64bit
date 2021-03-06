      SUBROUTINE MINPGP
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill PGPC bank from DGPC.
C
C     Author: Stephen Haywood      02-Mar-93
C-----------------------------------------------------------------------
C
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "pgpcjj.h"
#include "dgpcjj.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up DGPC bank.
C
      KDGPC = NLINK('DGPC',0)
      IF (KDGPC.LE.0) RETURN
      NDGPC = LROWS(KDGPC)
      IF (NDGPC.LE.0) RETURN
C
C++   Create PGPC bank.
C
      LEN = LMHLEN + LPGPCA * NDGPC
      CALL AUBOS('PGPC',0,LEN, KPGPC,IGARB)
      CALL BLIST(IW,'S+','PGPC')
      IF (IGARB.GE.2) THEN
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KDGPC = NLINK('DGPC',0)
      ENDIF
      IW(KPGPC+LMHCOL) = LPGPCA
      IW(KPGPC+LMHROW) = NDGPC
C
C++   Fill PGPC bank.
C
      DO 100 I=1,NDGPC
         RW(KROW(KPGPC,I)+JPGPEC) = FLOAT(ITABL(KDGPC,I,JDGPEC))/EFACTM
         RW(KROW(KPGPC,I)+JPGPTC) = FLOAT(ITABL(KDGPC,I,JDGPTC))/AFACTM
         RW(KROW(KPGPC,I)+JPGPPC) = FLOAT(ITABL(KDGPC,I,JDGPPC))/AFACTM
         RW(KROW(KPGPC,I)+JPGPR1) = FLOAT(ITABL(KDGPC,I,JDGPR1))/1000.
         RW(KROW(KPGPC,I)+JPGPR2) = FLOAT(ITABL(KDGPC,I,JDGPR2))/1000.
         RW(KROW(KPGPC,I)+JPGPF4) = FLOAT(ITABL(KDGPC,I,JDGPF4))/1000.
         MDIST = ITABL(KDGPC,I,JDGPDM)
         IF (MDIST.EQ.-1) MDIST = +99990
         RW(KROW(KPGPC,I)+JPGPDM) = FLOAT(MDIST)/10.
         RW(KROW(KPGPC,I)+JPGPST) = FLOAT(ITABL(KDGPC,I,JDGPST))
         IW(KROW(KPGPC,I)+JPGPQU) = ITABL(KDGPC,I,JDGPQU)
         RW(KROW(KPGPC,I)+JPGPQ1) = FLOAT(ITABL(KDGPC,I,JDGPQ1))/100.
         RW(KROW(KPGPC,I)+JPGPQ2) = FLOAT(ITABL(KDGPC,I,JDGPQ2))/100.
         RW(KROW(KPGPC,I)+JPGPM1) = FLOAT(ITABL(KDGPC,I,JDGPM1))/100.
         RW(KROW(KPGPC,I)+JPGPM2) = FLOAT(ITABL(KDGPC,I,JDGPM2))/100.
         RW(KROW(KPGPC,I)+JPGPMA) = FLOAT(ITABL(KDGPC,I,JDGPMA))/EFACTM
         RW(KROW(KPGPC,I)+JPGPER) = FLOAT(ITABL(KDGPC,I,JDGPER))/EFACTM
         RW(KROW(KPGPC,I)+JPGPTR) = FLOAT(ITABL(KDGPC,I,JDGPTR))/AFACTM
         RW(KROW(KPGPC,I)+JPGPPR) = FLOAT(ITABL(KDGPC,I,JDGPPR))/AFACTM
         IW(KROW(KPGPC,I)+JPGPPE) = ITABL(KDGPC,I,JDGPPE)
  100 CONTINUE
C
      RETURN
      END
#endif
