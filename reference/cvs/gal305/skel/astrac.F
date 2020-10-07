*DK astrac
      SUBROUTINE ASTRAC
C ----------------------------------------------------------------------
C. - F.RANJARD - 850328
C! Tracking steering routine
C. - Called from    ASPEVE                           from this .HLB
C -              ASCTRA                         from this .HLB
C - Calls         BDROP, BLIST                    from BOS77.lib
C.
C -----------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
      DATA IFI /0/
C ----------------------------------------------------------------------
C
C - initialize random number if required
      IF (IFI.EQ.0) THEN
         IF (IRNDJO(1,2).NE.0) CALL RDMIN (IRNDJO(1,2))
         IFI = 1
      ENDIF
      CALL RDMOUT (IRNDJO(1,2))
C
C - Track one event using standard version from GEANT3
      CALL GTREVE
C
C - Close tracking
      CALL ASCTRA
C
      END
#endif
