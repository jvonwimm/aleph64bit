       SUBROUTINE EDLIST ( JVAL,IVAL,KVAL,ID0,IOK)
C----------------------------------------------------------------------
C!  - Check same storey not twice in dead list
C      Author M.N Minard          12.04.90
C      Input : Jval Theta row index
C              Ival Phi row index
C              Kval Stack number
C              ID0  # elements in EDST
C      Output: IOK = 0 Non existing storey
C                  = 1 Existing storey
C?
C!======================================================================
#ifndef DOC
#include "edstjj.h"
#include "bcs.h"
       EXTERNAL NAMIND
#include "bmacro.h"
       IOK = 0
       KEDST = IW(NAMIND('EDST'))
       IF ( KEDST.EQ.0) GO TO 999
C
C-     Loop on EDST bank
C
C      PRINT * , 'LOOP ON EDST' ,ID0
       DO 10 IEDST = 1, ID0
       INDSB = KROW ( KEDST,IEDST)
       JVAL0 = IW(INDSB+JEDSTJ)
       IVAL0 = IW(INDSB+JEDSFI)
       KVAL0 = IW(INDSB+JEDSDK)
       IF ( IVAL.EQ.IVAL0.AND.KVAL.EQ.KVAL0.AND.JVAL.EQ.JVAL0) THEN
          IOK = 1
          GO TO 999
       ENDIF
 10    CONTINUE
 999   RETURN
       END
#endif
