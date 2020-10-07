      SUBROUTINE FINIRU
C----------------------------------------------------------------------
C!Initialise Global track fit stuff
C
C   Author         L. Garrido    10-may-1988
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "zpfrjj.h"
      CALL AUBOS('ZPFR',0,LMHLEN+1*LZPFRA,KZPFR,IGAR)
      IF(IGAR.EQ.2) RETURN
      IW(KZPFR+LMHCOL)= LZPFRA
      IW(KZPFR+LMHROW)= 1
      RW(KZPFR+LMHLEN+JZPFC1)=25000.
      RW(KZPFR+LMHLEN+JZPFC2)=25000.
      END
#endif
