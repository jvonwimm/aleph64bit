      INTEGER FUNCTION VMCMOD(JMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Returns mcode of global module number
C
C - Joe Rothberg, August 1995
C
C - Input:
C   JMOD   / I  global module number
C
C - Output:
C           returns mcode
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C ----------------------------------------------------------------------
#include "vduefc.h"
#include "vglobl.h"
#include "vsltco.h"
#include "vgindx.h"
#include "vmcode.h"
C ---------------------------------------------------------------------
C Arguments
      INTEGER JMOD
C Local variables
C Functions
C ------------------------------------------------------
      VMCMOD = VDERR
C
      IF(JMOD .GE. 1 .AND. JMOD .LE. NMODUL) THEN
C
        VMCMOD = MCODE_FROM_JMOD(JMOD)
C
      ENDIF
C
      RETURN
      END
#endif
