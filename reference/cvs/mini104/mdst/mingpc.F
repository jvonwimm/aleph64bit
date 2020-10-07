      SUBROUTINE MINGPC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill photon bank DGPC for Mini-DST.
C
C     Author: Stephen Haywood      02-Mar-93
C
C     Input  : PGPC bank
C     Output : DGPC bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
C
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "pgpcjj.h"
#include "dgpcjj.h"
#include "bmacro.h"
C
C++   Pick up PGPC bank.
C
      KPGPC = NLINK('PGPC',0)
      IF (KPGPC.LE.0) RETURN
      NPGPC = LROWS(KPGPC)
      IF (NPGPC.LE.0) RETURN
C
C++   Create the DGPC bank.
C
      NDGPC = NPGPC
      LEN = LMHLEN + LDGPCA * NDGPC
      CALL AUBOS('DGPC',0,LEN, KDGPC,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINGPC: Cannot create DGPC bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPGPC = NLINK('PGPC',0)
      ENDIF
      IW(KDGPC+LMHCOL) = LDGPCA
      IW(KDGPC+LMHROW) = NDGPC
C
C++   Loop over PGPC storing information in DGPC.
C
      DO 100 I=1,NPGPC
         IW(KROW(KDGPC,I)+JDGPEC) = NINT(EFACTM * RTABL(KPGPC,I,JPGPEC))
         IW(KROW(KDGPC,I)+JDGPTC) = NINT(AFACTM * RTABL(KPGPC,I,JPGPTC))
         IW(KROW(KDGPC,I)+JDGPPC) = NINT(AFACTM * RTABL(KPGPC,I,JPGPPC))
         IW(KROW(KDGPC,I)+JDGPR1) = NINT(1000. * RTABL(KPGPC,I,JPGPR1))
         IW(KROW(KDGPC,I)+JDGPR2) = NINT(1000. * RTABL(KPGPC,I,JPGPR2))
         IW(KROW(KDGPC,I)+JDGPF4) = NINT(1000. * RTABL(KPGPC,I,JPGPF4))
         DISTM = RTABL(KPGPC,I,JPGPDM)
         IF (DISTM.GT.1000.) DISTM = -0.1
         IW(KROW(KDGPC,I)+JDGPDM) = NINT(10. * DISTM)
         IW(KROW(KDGPC,I)+JDGPST) = NINT(RTABL(KPGPC,I,JPGPST))
         IW(KROW(KDGPC,I)+JDGPQU) = ITABL(KPGPC,I,JPGPQU)
         IW(KROW(KDGPC,I)+JDGPQ1) = NINT(100. * RTABL(KPGPC,I,JPGPQ1))
         IW(KROW(KDGPC,I)+JDGPQ2) = NINT(100. * RTABL(KPGPC,I,JPGPQ2))
         IW(KROW(KDGPC,I)+JDGPM1) = NINT(100. * RTABL(KPGPC,I,JPGPM1))
         IW(KROW(KDGPC,I)+JDGPM2) = NINT(100. * RTABL(KPGPC,I,JPGPM2))
         IW(KROW(KDGPC,I)+JDGPMA) = NINT(EFACTM * RTABL(KPGPC,I,JPGPMA))
         IW(KROW(KDGPC,I)+JDGPER) = NINT(EFACTM * RTABL(KPGPC,I,JPGPER))
         IW(KROW(KDGPC,I)+JDGPTR) = NINT(AFACTM * RTABL(KPGPC,I,JPGPTR))
         IW(KROW(KDGPC,I)+JDGPPR) = NINT(AFACTM * RTABL(KPGPC,I,JPGPPR))
         IW(KROW(KDGPC,I)+JDGPPE) = ITABL(KPGPC,I,JPGPPE)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DGPC')
C
      RETURN
      END
#endif
