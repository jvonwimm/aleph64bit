      SUBROUTINE PRIMPA
C -----------------------------------------------------------------
C - O.Callot and F.Ranjard - 860929
C! Print IMPAct Bank in Readable Format
#ifndef DOC
      SAVE
#include "bcs.h"
      CHARACTER*4 CHAINT
#include "bmacro.h"
C --------------------------------------------------------------
C
      LOUT = IW(6)
C
      NAMI = NAMIND('IMPA')
      IF (NAMI .EQ. 0) THEN
         WRITE (LOUT,'(/1X,''+++PRIMPA+++ NO IMPA bank - RETURN'')')
         RETURN
      ENDIF
C
      JIMPA = NAMI + 1
 50   JIMPA = IW(JIMPA-1)
      IF(JIMPA.NE.0) THEN
         WRITE(LOUT,1000) IW(JIMPA-2),LROWS(JIMPA)
         KIMPA = JIMPA +LMHLEN
         DO 60 J=1,LROWS(JIMPA)
            WRITE(LOUT,1010) CHAINT(IW(KIMPA+1))
     &                      ,(RW(KIMPA+K),K=2,LCOLS(JIMPA))
   60    KIMPA = KIMPA + LCOLS(JIMPA)
         GOTO 50
      ENDIF
C
      RETURN
 1000 FORMAT(/' +++PRIMPA+++ track# ',I4,3X,I2,' Impacts')
 1010 FORMAT(10X,A4,2X,3F10.3,2X,3F8.4,2X,F10.4)
      END
#endif
