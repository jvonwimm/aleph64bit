*DK askine
      SUBROUTINE ASKINE
C ----------------------------------------------------------------------
C. - F.RANJARD - 850328                           modified - 871029
C! Kinematics steering routine
C. - TKINJO = LUND     LUND generator     (default)
C.            SJET     LUND generator , single jet
C.            PART     single particle
C.            other    user generator
C. - Called from    ASPEVE                       from  this .HLB
C. - Calls          ASKLUN , ASKSIN              from  this .HLB
C.                  ASKUSE                       from user library
C.                  there is a dummy subroutine ASKUSE in this .HLB
C.                  to avoid unsatisfied external.
C.
C -----------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "kincom.h"
#include "bcs.h"
#include "gcnum.h"
      DATA IFI /0/
C ----------------------------------------------------------------------
C
C - Drop existing banks
C
      CALL BDROP (IW,'VERTKINE')
C - Add kinematics banks to the 'E' list
      CALL BLIST (IW,'E+','VERTKINE')
C
C - keep 1st random number
C
      IF (IFI .EQ. 0) THEN
         IF (IRNDJO(1,1) .NE. 0) CALL RDMIN (IRNDJO(1,1))
         IFI = 1
      ENDIF
      CALL RDMOUT (IRNDJO(1,1))
C
C - Call the event generator according to the process chosen
C
      ISTAKI = 0
      WEITKI = 1
C
      IF (IPROKI .LT. LOFFMC*4) THEN
C -      TKINJO = PART            Single particle generation
         CALL ASKSIN (IDEVKI,ISTAKI,NITRKI,NIVXKI,ECMSKI,WEITKI)
      ELSE
C -      TKINJO = anything else   User generator
         CALL ASKUSE (IDEVKI,ISTAKI,NITRKI,NIVXKI,ECMSKI,WEITKI)
      ENDIF
C
C - skip the event if the status word is NOT 0
C
      IF (ISTAKI .NE. 0) CALL ALTELL (' ',10,'NEXT')
C
C - end
C
 999  CONTINUE
      END
#endif
