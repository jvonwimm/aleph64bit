      INTEGER FUNCTION VSMMOD (MMOD,ISMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Returns serial number of signed module number, VDET95
C
C - Joe Rothberg, August 1995
C
C - Input:
C   MMOD   / I  signed global module number
C
C - Output:
C   ISMOD   / I  Serial number of Module
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C ----------------------------------------------------------------------
#include "vduefc.h"
#include "vglobl.h"
#include "vsltco.h"
C ---------------------------------------------------------------------
C Arguments
      INTEGER MMOD, ISMOD
C Local variables
      INTEGER IFACS, JMOD, JFAC
C Functions
      INTEGER VJMODM, VJFACM, VFACSN
C ------------------------------------------------------
      VSMMOD = VDERR
C
      IF(MMOD.GE.-NSLOTS.AND.MMOD.LE.NSLOTS.AND.MMOD.NE.0) THEN
C
            JMOD = VJMODM(MMOD)
            JFAC = VJFACM(JMOD)
C find face serial number
            IFACS = VFACSN(JFAC)
C
            IF (IFACS .GE. 1 .AND. IFACS .LE. maxface) THEN
               IF(MMOD .GT. 0)   ISMOD = MODPOS(IFACS)
               IF(MMOD .LT. 0)   ISMOD = MODNEG(IFACS)
               VSMMOD = VDOK
            ENDIF
      ENDIF
C
      RETURN
      END
#endif
