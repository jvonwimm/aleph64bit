      INTEGER FUNCTION VFASMD (JFAC,TXFA,ISMN,ISMP)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C! Returns serial number of modules in global face JFAC and face name.VDET95
C
C - Joe Rothberg, August 1995
C
C - Input:
C   JFAC   / I  Global Face number
C
C - Output:
C   TXFA   / C*4  face name
C   ISMN   / I  Serial number of Module on side B (negative z)
C   ISMP   / I  Serial number of Module on side A (positive z)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C ----------------------------------------------------------------------
#include "vduefc.h"
#include "vglobl.h"
#include "vsltco.h"
C ---------------------------------------------------------------------
C Arguments
      INTEGER JFAC, ISMN, ISMP
      CHARACTER*4 TXFA
C Local variables
      INTEGER IFACS
C Functions
      INTEGER VFACSN
C ------------------------------------------------------
      VFASMD = VDERR
C
      IF(JFAC .GE. 1 .AND. JFAC .LE. NSLOTS) THEN
C
C find face serial number
            IFACS = VFACSN(JFAC)
C
            IF (IFACS .GE. 1 .AND. IFACS .LE. maxface) THEN
                TXFA = FACEC(IFACS)
                ISMN = MODNEG(IFACS)
                ISMP = MODPOS(IFACS)
                VFASMD = VDOK
            ENDIF
      ENDIF
C
      RETURN
      END
#endif
