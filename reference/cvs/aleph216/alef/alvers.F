      SUBROUTINE ALVERS (ALEFV)
C --------------------------------------------------------------------
C - F.Ranjard - 870401
C! Return the Alephlib version # in ALEFV
C  Print the version # on IW(6)
C--------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "version.h"
C ------------------------------------------------------------------
      ALEFV = ALEVER
C
      LOUT = IW(6)
      IF (LOUT.NE.0) THEN
         WRITE (LOUT,'(/1X,''+++ALVERS+++ Alephlib version '',F6.2)')
     &          ALEFV
      ENDIF
C
      END
#endif
