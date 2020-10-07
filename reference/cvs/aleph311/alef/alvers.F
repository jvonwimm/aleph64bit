      SUBROUTINE ALVERS (ALEFV)
C --------------------------------------------------------------------
C - F.Ranjard - 870401
C   Modified M.Cattaneo 981028 - Print out version number only once per job
C! Return the Alephlib version # in ALEFV
C  Print the version # on IW(6)
C--------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "version.h"
      LOGICAL FIRST /.TRUE./
C ------------------------------------------------------------------
      ALEFV = ALEVER
C
      IF ( FIRST ) THEN
        LOUT = IW(6)
        IF (LOUT.NE.0) THEN
         WRITE (LOUT,'(/1X,''+++ALVERS+++ Alephlib version '',F6.2)')
     &          ALEFV
        ENDIF
        FIRST = .FALSE.
      ENDIF
C
      END
#endif
