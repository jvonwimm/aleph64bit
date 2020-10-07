      SUBROUTINE ESLAS(ICLC,NTK,IKEPT)
C***********************************************************************
C!Select tracks for EcObject association                               *
C!                                                                     *
C!  One track can be associated to several EcClusters;                 *
C!  we want only one EcObject per track.                               *
C!                                                                     *
C!   Action : select only the first cluster for each track             *
C!            This will be the last one in the chain, since            *
C!            it starts from the end                                   *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870819                                    *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETC2,ERL3                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "etc2jj.h"
#include "erl3jj.h"
#include "ecnamc.h"
#include "bmacro.h"
      KETC2=IW(NAETC2)
      KERL3=IW(NAERL3)
      NREL=ITABL(KETC2,NTK,JETCER)
   20 NRNXT=ITABL(KERL3,NREL,JERLER)
         IF(NRNXT.EQ.0)GO TO 10
      NREL=NRNXT
      GO TO 20
   10 CONTINUE
      ICLL=ITABL(KERL3,NREL,JERLEC)
      IKEPT=0
      IF(ICLL.EQ.ICLC)IKEPT=1
      RETURN
      END
#endif
