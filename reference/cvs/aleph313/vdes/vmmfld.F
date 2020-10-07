      REAL FUNCTION VMMFLD (MMOD)
C ----------------------------------------------------------------------
CKEY VDETDES FIELD / USER
C!  Return magnetic field in local (a,b,c) coordinates
C - Joe Rothberg, 15 January 1994
C
C - Input:
C   MMOD   / I  Signed global module index
C
C - Output:
C   VMMFLD / R  Component of magnetic field along +a direction (Tesla)
C               = 0 if error
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
C
C     Functions:
      REAL ALFIEL
C
C     Local variables
      INTEGER MMOD
      INTEGER MODSN
      REAL FIELD
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      IF (MMOD.GE.-NSLOTS .AND. MMOD.LE.NSLOTS .AND. MMOD.NE.0) THEN
C
C sign of z (module)
        MODSN = ISIGN(1,MMOD)
C
C temporary
        FIELD = ALFIEL(0.)
        VMMFLD = MODSN*FIELD
C
      ELSE
C
C     argument JWAF out of range
C
        VMMFLD = 0.
C
      ENDIF
C
      RETURN
      END
#endif
