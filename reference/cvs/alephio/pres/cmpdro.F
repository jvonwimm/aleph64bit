      SUBROUTINE CMPDRO
C----------------------------------------------------------------------
C!    Drops work banks used by the compressor
CKEY PRESS CMPDRO COMPRESS /USER
C!
C!    AUTHOR: D.Harvatis    MAY 89
C!    CALLS : WDROP
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "cmpinf.h"
C----------------------------------------------------------------------
      IF (NUMBNK.EQ.0) RETURN
      DO 10 IB=1,NUMBNK
   10     CALL WDROP(IW,INDEXW(IB))
      RETURN
      END
#endif
