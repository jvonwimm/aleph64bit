      SUBROUTINE MINFZF
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill FZFR from DFMC.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : DFMC bank
C     Output : FZFR bank
C
C     Called by MINFIL
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "fzfrjj.h"
#include "dfmcjj.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up DFMC bank.
C
      KDFMC = NLINK('DFMC',0)
      IF (KDFMC.LE.0) RETURN
      NDFMC = LROWS(KDFMC)
      IF(NDFMC.LE.0) RETURN
C
C++   Create the FZFR bank.
C
      LEN = LMHLEN + LFZFRA * NDFMC
      CALL AUBOS('FZFR',0,LEN, KFZFR,IGARB)
      CALL BLIST(IW,'S+','FZFR')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDFMC = NLINK('DFMC',0)
      ENDIF
      IW(KFZFR+LMHCOL) = LFZFRA
      IW(KFZFR+LMHROW) = NDFMC
C
C++   Fill FZFR bank.
C
      DO I=1,NDFMC
         RW(KROW(KFZFR,I)+JFZFZF) = FLOAT(ITABL(KDFMC,I,JDFMZF))/1000.
      ENDDO
C
      RETURN
      END
#endif
