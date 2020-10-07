      SUBROUTINE MINGID
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill photon bank DGID for Mini-DST.
C
C     Author: Stephen Haywood      02-Mar-93
C
C     Input  : PGID bank
C     Output : DGID bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
C
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "pgidjj.h"
#include "dgidjj.h"
#include "bmacro.h"
C
C++   Pick up PGID bank.
C
      KPGID = NLINK('PGID',0)
      IF (KPGID.LE.0) RETURN
      NPGID = LROWS(KPGID)
      IF (NPGID.LE.0) RETURN
C
C++   Create the DGID bank.
C
      NDGID = NPGID
      LEN = LMHLEN + LDGIDA * NDGID
      CALL AUBOS('DGID',0,LEN, KDGID,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINGID: Cannot create DGID bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPGID = NLINK('PGID',0)
      ENDIF
      IW(KDGID+LMHCOL) = LDGIDA
      IW(KDGID+LMHROW) = NDGID
C
C++   Loop over PGID storing information in DGID.
C++   Note: non-standard name for index for relation between DGIG and
C++   DECO in order to avoid clash with depth estimator.
C
      DO 100 I=1,NPGID
         IW(KROW(KDGID,I)+JDGIIF) = ITABL(KPGID,I,JPGIIF)
         IW(KROW(KDGID,I)+JDGIDE) = NINT(100. * RTABL(KPGID,I,JPGIDE))
         IW(KROW(KDGID,I)+JDGICM) = NINT(100. * RTABL(KPGID,I,JPGICM))
         IW(KROW(KDGID,I)+JDGIM1) = NINT(100. * RTABL(KPGID,I,JPGIM1))
         IW(KROW(KDGID,I)+JDGIM2) = NINT(100. * RTABL(KPGID,I,JPGIM2))
         IW(KROW(KDGID,I)+JDGIM3) = NINT(EFACTM * RTABL(KPGID,I,JPGIM3))
         IW(KROW(KDGID,I)+JDGICE) = NINT(EFACTM * RTABL(KPGID,I,JPGICE))
         IW(KROW(KDGID,I)+JDGITH) = NINT(AFACTM * RTABL(KPGID,I,JPGITH))
         IW(KROW(KDGID,I)+JDGIPH) = NINT(AFACTM * RTABL(KPGID,I,JPGIPH))
         IW(KROW(KDGID,I)+JDGIPE) = ITABL(KPGID,I,JPGIPE)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DGID')
C
      RETURN
      END
#endif
