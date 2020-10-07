      SUBROUTINE MINPOB
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Hcal digital pattern object DPOB for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PPOB bank
C     Output : DPOB bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "ppobjj.h"
#include "dpobjj.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up PPOB bank.
C
      KPPOB = NLINK('PPOB',0)
      IF(KPPOB.GT.0) THEN
         NPPOB = LROWS(KPPOB)
      ELSE
         NPPOB = 0
      ENDIF
      IF(NPPOB.LE.0) RETURN
C
C++   Create DPOB bank.
C
      NDPOB = NPPOB
      LEN = LMHLEN + LDPOBA * NDPOB
      CALL AUBOS('DPOB',0,LEN, KDPOB,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINPOB: Cannot create DPOB bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPPOB = NLINK('PPOB',0)
      ENDIF
      IW(KDPOB+LMHCOL) = LDPOBA
      IW(KDPOB+LMHROW) = NDPOB
C
C++   Loop over PPOB storing information in DPOB.
C
      DO 100 I=1,NPPOB
         IW(KROW(KDPOB,I)+JDPOE0) = NINT(EFACTM * RTABL(KPPOB,I,JPPODE))
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DPOB')
C
      RETURN
      END
#endif
