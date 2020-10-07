*DK astrig
      SUBROUTINE ASTRIG
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 86/08/08  FOR GALEPH 13.0
C. - Modified : A. Putzer  - 95/01/18  FOR GALEPH 300.
C.
C! - Apply trigger conditions
C.
C? - This routine acts as the steering routine for the application
C?   of the Level1 and Level2 trigger
C?
C? - Level 1        EC, HC, LC , Si and IT are used
C?
C? - Level 2        TP is used for Level 1 track triggers
C.
C. - Called by      ASPEVE                        from this .HLB
C.
C. - Calls          X1TRIG                        from ALEPHLIB
C. -                X2TRIG                        from ALEPHLIB
C ------------------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
C ----------------------------------------------------------------------
C
C - Level 1
C
      CALL X1TRIG
      IF (FDEBJO .AND. IPRIJO(11).NE.0) CALL X1PRNT
C
C - Level 2
C
      CALL X2TRIG(IRET)
      IF (IRET.NE.0) RETURN
      IF (FDEBJO .AND. IPRIJO(11).NE.0) CALL X2PRIN ('EVEN')
C
      END
#endif
