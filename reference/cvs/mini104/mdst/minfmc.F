      SUBROUTINE MINFMC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill fragmentation MC bank DFMC for Mini-DST.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : FZFR bank
C     Output : DFMC bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "fzfrjj.h"
#include "dfmcjj.h"
#include "bmacro.h"
C
C++   Pick up FZFR bank.
C
      KFZFR = NLINK('FZFR',0)
      IF (KFZFR.LE.0) RETURN
      NFZFR = LROWS(KFZFR)
      IF(NFZFR.LE.0) RETURN
C
C++   Create the DFMC bank.
C
      LEN = LMHLEN + LDFMCA * NFZFR
      CALL AUBOS('DFMC',0,LEN, KDFMC,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINFMC: Cannot create DFMC bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KFZFR = NLINK('FZFR',0)
      ENDIF
      IW(KDFMC+LMHCOL) = LDFMCA
      IW(KDFMC+LMHROW) = NFZFR
C
C++   Copy FZFR to DFMC, integerising fragmention fractions.
C
      DO I=1,NFZFR
         IW(KROW(KDFMC,I)+JDFMZF) = NINT(1000 * RTABL(KFZFR,I,JFZFZF))
      ENDDO
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DFMC')
C
      RETURN
      END
#endif
