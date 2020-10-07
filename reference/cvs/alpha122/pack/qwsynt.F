      SUBROUTINE QWSYNT (CHAR)
CKEY CARDS SYNTAX ERROR /INTERNAL
C----------------------------------------------------------------------
C! print syntax error message
C
C called from program initialisation routines
C                                                   H.Albrecht 13.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER * (*) CHAR
C----------------------------------------------------------------------
      IF (KSTATU)  10,30,20
   10 XSYNTX = .TRUE.
   20 CALL QWMESS (CHAR)
      GO TO 90
   30 CALL QMTERM (CHAR)
   90 CALL QWMESS ('_QWSYNT_ ------------ Syntax ---------------')
      END
#endif
