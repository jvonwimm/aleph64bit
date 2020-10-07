      INTEGER FUNCTION VSMMTX (TXTM,ISMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Returns serial number of module text name, VDET95
C
C - Joe Rothberg, August 1995
C
C - Input:
C   TXTM   / I  module text name
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
      INTEGER  ISMOD
      CHARACTER*4 TXTM
C Local variables
      INTEGER IRET, ILAY, IFAC, IMOD, IFACS, JMOD, JFAC
C Functions
      INTEGER VITEXI, VJMODI, VJFACM, VFACSN
C ------------------------------------------------------
      VSMMTX = VDERR
C
      IRET = VITEXI(TXTM,ILAY,IFAC,IMOD)
          IF(IRET .EQ. VDOK) THEN
             IRET = VJMODI(ILAY,IFAC,IMOD,JMOD)
             JFAC = VJFACM(JMOD)
C find face serial number
             IFACS = VFACSN(JFAC)
C
             IF (IFACS .GE. 1 .AND. IFACS .LE. maxface) THEN
                IF(IMOD .EQ. 2)   ISMOD = MODPOS(IFACS)
                IF(IMOD .EQ. 1)   ISMOD = MODNEG(IFACS)
                VSMMTX = VDOK
             ENDIF
           ENDIF
C
      RETURN
      END
#endif
