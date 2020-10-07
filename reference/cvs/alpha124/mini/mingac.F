      SUBROUTINE MINGAC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill photon bank DGAC for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    17-Oct-94
C
C     Input  : PGAC bank
C     Output : DGAC bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
#include "pgacjj.h"
#include "dgacjj.h"
C!    set of intrinsic functions to handle BOS banks
#include "bmacro.h"
C-----------------------------------------------------------------------
C++   Pick up PGAC bank.
C
      KPGAC = NLINK('PGAC',0)
      IF (KPGAC.LE.0) RETURN
      NPGAC = LROWS(KPGAC)
      IF (NPGAC.LE.0) RETURN
C
C++   Create the DGAC bank.
C
      NDGAC = NPGAC
      LEN = LMHLEN + LDGACA * NDGAC
      CALL AUBOS('DGAC',0,LEN, KDGAC,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINGAC: Cannot create DGAC bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPGAC = NLINK('PGAC',0)
      ENDIF
      IW(KDGAC+LMHCOL) = LDGACA
      IW(KDGAC+LMHROW) = NDGAC
C
C++   Loop over PGAC storing information in DGAC.
C
      DO 100 I=1,NDGAC
         IW(KROW(KDGAC,I)+JDGAEC) = NINT(EFACTM * RTABL(KPGAC,I,JPGAEC))
         IW(KROW(KDGAC,I)+JDGATC) = NINT(AFACTM * RTABL(KPGAC,I,JPGATC))
         IW(KROW(KDGAC,I)+JDGAPC) = NINT(AFACTM * RTABL(KPGAC,I,JPGAPC))
         IW(KROW(KDGAC,I)+JDGAR1) = NINT(1000. * RTABL(KPGAC,I,JPGAR1))
         IW(KROW(KDGAC,I)+JDGAR2) = NINT(1000. * RTABL(KPGAC,I,JPGAR2))
         IW(KROW(KDGAC,I)+JDGAF4) = NINT(1000. * RTABL(KPGAC,I,JPGAF4))
         DISTM = RTABL(KPGAC,I,JPGADM)
         IF (DISTM.GT.1000.) DISTM = -0.1
         IW(KROW(KDGAC,I)+JDGADM) = NINT(10. * DISTM)
         IW(KROW(KDGAC,I)+JDGAST) = ITABL(KPGAC,I,JPGAST)
         IW(KROW(KDGAC,I)+JDGAQU) = ITABL(KPGAC,I,JPGAQU)
         IW(KROW(KDGAC,I)+JDGAQ1) = NINT(100. * RTABL(KPGAC,I,JPGAQ1))
         IW(KROW(KDGAC,I)+JDGAQ2) = NINT(100. * RTABL(KPGAC,I,JPGAQ2))
         IW(KROW(KDGAC,I)+JDGAM1) = NINT(100. * RTABL(KPGAC,I,JPGAM1))
         IW(KROW(KDGAC,I)+JDGAM2) = NINT(100. * RTABL(KPGAC,I,JPGAM2))
         IW(KROW(KDGAC,I)+JDGAMA) = NINT(EFACTM * RTABL(KPGAC,I,JPGAMA))
         IW(KROW(KDGAC,I)+JDGAER) = NINT(EFACTM * RTABL(KPGAC,I,JPGAER))
         IW(KROW(KDGAC,I)+JDGATR) = NINT(AFACTM * RTABL(KPGAC,I,JPGATR))
         IW(KROW(KDGAC,I)+JDGAPR) = NINT(AFACTM * RTABL(KPGAC,I,JPGAPR))

         IW(KROW(KDGAC,I)+JDGAEF) = NINT(AFACTM * RTABL(KPGAC,I,JPGAEF))
         IW(KROW(KDGAC,I)+JDGAGC) = NINT(AFACTM * RTABL(KPGAC,I,JPGAGC))
         IW(KROW(KDGAC,I)+JDGAZS) = NINT(AFACTM * RTABL(KPGAC,I,JPGAZS))
         IW(KROW(KDGAC,I)+JDGAPL) = NINT(AFACTM * RTABL(KPGAC,I,JPGAPL))
         IW(KROW(KDGAC,I)+JDGAPF) = NINT(AFACTM * RTABL(KPGAC,I,JPGAPH))
         IW(KROW(KDGAC,I)+JDGAPN) = ITABL(KPGAC,I,JPGAPN)
         IW(KROW(KDGAC,I)+JDGAFA) = ITABL(KPGAC,I,JPGAFA)

         IW(KROW(KDGAC,I)+JDGAPE) = ITABL(KPGAC,I,JPGAPE)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DGAC')
C
      RETURN
      END
#endif
