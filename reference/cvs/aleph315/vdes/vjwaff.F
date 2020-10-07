      INTEGER FUNCTION VJWAFF (JFAC,IWFF,JWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates global wafer from global face and wafer-in-face
C - Joe Rothberg, December 1993
C
C - Input:
C   JFAC   / I  Global face index
C   IWFF   / I  Local wafer-in-face index
C
C - Output:
C   VJWAFF / I  = VDOK if successful
C               = VDERR if error occurred
C   JWAF   / I  Global wafer index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vzpwco.h"
#include "vsltco.h"
#include "vgindx.h"
C
      INTEGER JWAF, JFAC, IWFF
C
C   local variables
      INTEGER IFAC, ILAY
      INTEGER STATUS
C
C functions
      INTEGER VIFACI
C
C ----------------------------------------------------------------------
C
      IF (JFAC .GE. 1 .AND. JFAC .LE. NSLOTS .AND.
     >        IWFF .GE. 1 .AND. IWFF .LE. NWAFEF) THEN
C
        STATUS = VIFACI(JFAC,ILAY,IFAC)
        JWAF = IJWFFR(ILAY,IFAC,IWFF)
C
        VJWAFF = VDOK
      ELSE
        VJWAFF =  VDERR
      ENDIF
C
      RETURN
      END
#endif
