      INTEGER FUNCTION VPHSTM (IVIEW,NPHSTR,PPITCH)
C ----------------------------------------------------------------------
CKEY VDETDES STRIP / USER
C!  Returns the number and pitch of the physical strips on a wafer
C - Steve Wasserbaech, February 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VPHSTM / I  = VDOK if successful;
C               = VDERR if IVIEW is invalid.
C   NPHSTR / I  Number of physical strips
C   PPITCH / R  Physical strip pitch (cm)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vwgeco.h"
C
C     Arguments:
      INTEGER IVIEW, NPHSTR
      REAL PPITCH
C
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
        NPHSTR = NZSTRP
        PPITCH = STPITZ
        VPHSTM = VDOK
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        NPHSTR = NPSTRP
        PPITCH = STPITP
        VPHSTM = VDOK
C
      ELSE
C
C     invalid view:
        NPHSTR = 0
        PPITCH = 0.
        VPHSTM = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
