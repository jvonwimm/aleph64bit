      SUBROUTINE MINSIC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Sical bank DSIC for Mini-DST.
C
C     Author: Stephen Haywood      24-May-93
C
C     Input  : SILU bank
C     Output : DSIC bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
C
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "silujj.h"
#include "dsicjj.h"
#include "bmacro.h"
C
C++   Pick up SILU bank.
C
      KSILU = NLINK('SILU',0)
      IF (KSILU.LE.0) RETURN
      NSILU = LROWS(KSILU)
      IF (NSILU.LE.0) RETURN
C
C++   Create the DSIC bank.
C
      NDSIC = NSILU
      LEN = LMHLEN + LDSICA * NDSIC
      CALL AUBOS('DSIC',0,LEN, KDSIC,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINSIC: Cannot create DSIC bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KSILU = NLINK('SILU',0)
      ENDIF
      IW(KDSIC+LMHCOL) = LDSICA
      IW(KDSIC+LMHROW) = NDSIC
C
C++   Loop over SILU storing information in DSIC.
C
      DO 100 I=1,NSILU
         IW(KROW(KDSIC,I)+JDSIIS) = ITABL(KSILU,I,JSILIS)
         IW(KROW(KDSIC,I)+JDSIEC) = NINT(EFACTM * RTABL(KSILU,I,JSILEC))
         IW(KROW(KDSIC,I)+JDSICR) = NINT(DFACTM * RTABL(KSILU,I,JSILCR))
         IW(KROW(KDSIC,I)+JDSICP) = NINT(AFACTM * RTABL(KSILU,I,JSILCP))
         IW(KROW(KDSIC,I)+JDSIZC) = NINT(DFACTM * RTABL(KSILU,I,JSILZC))
         IW(KROW(KDSIC,I)+JDSIXI) = NINT(DFACTM * RTABL(KSILU,I,JSILXI))
         IW(KROW(KDSIC,I)+JDSIYI) = NINT(DFACTM * RTABL(KSILU,I,JSILYI))
         IW(KROW(KDSIC,I)+JDSINP) = ITABL(KSILU,I,JSILNP)
         IW(KROW(KDSIC,I)+JDSIND) = ITABL(KSILU,I,JSILND)
  100 CONTINUE
C
C++   Add the bank to the Mini list. Also add SILH.
C
      CALL MINLIS('DSIC')
      CALL MINLIS('SILH')
C
      RETURN
      END
#endif
