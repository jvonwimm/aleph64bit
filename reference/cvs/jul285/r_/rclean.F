      SUBROUTINE RCLEAN
C--------------------------------------------------------------------
C! Delete some input banks according to data card REMI
C!                                                (REMove form Input)
C!
C!  Author        S. Orteu             19-JAN-1988
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "bosext.h"
      CHARACTER*4 CLBK
C--------------------------------------------------------------------
      KREMI = IW(NAMIND('REMI'))
      IF (KREMI.NE.0) THEN
        DO 10 J=1,IW(KREMI)
          CLBK = CHAINT(IW(KREMI+J))
          CALL BDROP(IW,CLBK)
   10   CONTINUE
      ENDIF
      END
#endif
