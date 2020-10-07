      INTEGER FUNCTION VMODMC(MCODE)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Returns global module number for mcode
C
C - Joe Rothberg, August 1995
C
C - Input:
C   MCODE   / I  module mcode
C
C - Output:
C           returns global module number, JMOD
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C ----------------------------------------------------------------------
#include "vduefc.h"
#include "vglobl.h"
#include "vsltco.h"
#include "vmcode.h"
C ---------------------------------------------------------------------
C Arguments
      INTEGER MCODE
C Local variables
C Functions
C ------------------------------------------------------
      VMODMC = VDERR
C
      IF(MCODE .GE. 1 .AND. MCODE .LE. max_MCODE) THEN
C
        VMODMC = JMOD_FROM_MCODE(MCODE)
C
      ENDIF
C
      RETURN
      END
#endif
