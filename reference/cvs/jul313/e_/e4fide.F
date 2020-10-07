      SUBROUTINE E4FIDE
C----------------------------------------------------------------------
C!  - FILL   E4DE BANK
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "e4dejj.h"
#include "e4com2.h"
#include "e4com0.h"
#include "rlunit.h"
#include "bmacro.h"
      NAE4DE=NAMIND('E4DE')
      KE4DE=IW(NAE4DE)
      IF(KE4DE.EQ.0)RETURN
      NE4DE=LROWS(KE4DE)
      IF(NE4DE.EQ.0)RETURN
      DO 1 K=1,3
      IF(IJDEAD(K).EQ.4)THEN
          IW(KROW(KE4DE,JTRAK)+K)=0
      ENDIF
  1   CONTINUE
      RETURN
      END
#endif
