      SUBROUTINE MINVRS
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill version information bank DVRS for Mini-DST.
C
C     Author: Stephen Haywood      01-Mar-93
C
C     Input  : from MINVSN and YV0V bank
C     Output : DVRS bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
C
#ifndef DOC
C
#include "bcs.h"
#include "dvrsjj.h"
#include "bmacro.h"
C
C++   Create the DVRS bank.
C
      LEN = LMHLEN + LDVRSA
      CALL AUBOS('DVRS',0,LEN, KDVRS,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINVRS: Cannot create DVRS bank'')')
         RETURN
      ENDIF
      IW(KDVRS+LMHCOL) = LDVRSA
      IW(KDVRS+LMHROW) = 1
C
C++   Save Mini version and bank number of YV0V.
C
      IW(KROW(KDVRS,1)+JDVRMV) = MINVSN(DUM)
      KYV0V = IW(NAMIND('YV0V'))
      IF (KYV0V.GT.0) THEN
         NR = IW(KYV0V-2)
      ELSE
         NR = -1
      ENDIF
      IW(KROW(KDVRS,1)+JDVRV0) = NR
C
C++   Add bank to the Mini list.
C
      CALL MINLIS('DVRS')
C
      RETURN
      END
#endif
