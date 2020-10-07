      INTEGER FUNCTION VSMJMD (JMOD,ISMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Returns serial number of global module number, VDET95
C
C - Joe Rothberg, August 1995
C
C - Input:
C   JMOD   / I  global module number
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
#include "vgindx.h"
C ---------------------------------------------------------------------
C Arguments
      INTEGER JMOD, ISMOD
C Local variables
      INTEGER IFACS, JFAC, MMOD
C Functions
      INTEGER VMMODJ, VJFACM, VFACSN
C ------------------------------------------------------
      VSMJMD = VDERR
C
      IF(JMOD .GE. 1 .AND. JMOD .LE. NMODUL) THEN
         MMOD = VMMODJ(JMOD)
         JFAC = VJFACM(JMOD)
C find face serial number
         IFACS = VFACSN(JFAC)
         IF (IFACS .GE. 1 .AND. IFACS .LE. maxface) THEN
            IF(MMOD .GT. 0)   ISMOD = MODPOS(IFACS)
            IF(MMOD .LT. 0)   ISMOD = MODNEG(IFACS)
            VSMJMD = VDOK
         ENDIF
      ENDIF
C
      RETURN
      END
#endif
