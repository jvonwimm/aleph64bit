      INTEGER FUNCTION VFASDP ()
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  dumps serial number of faces and modules, VDET95
C
C - Joe Rothberg, August 1995
C
C - Input:
C
C - Output:
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C ----------------------------------------------------------------------
#include "vduefc.h"
#include "vglobl.h"
#include "vsltco.h"
C ---------------------------------------------------------------------
C Local variables
      INTEGER I, JFAC, IFACS, ISMN, ISMP
C Functions
      INTEGER VFACSN
C ------------------------------------------------------
      VFASDP = VDOK
C -------------------------------------------------------------
      DO JFAC = 1, NSLOTS
C
C find face serial number
C
         IFACS = VFACSN(JFAC)
         IF (IFACS .GE. 1 .AND. IFACS .LE. maxface) THEN
             ISMN = MODNEG(IFACS)
             ISMP = MODPOS(IFACS)
         ENDIF
      ENDDO
C
      RETURN
      END
#endif
