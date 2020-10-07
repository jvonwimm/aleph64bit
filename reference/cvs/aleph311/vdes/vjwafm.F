      INTEGER FUNCTION VJWAFM (MMOD,IWAF,JWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Global wafer index from the mglobal module and local wafer indices
C - Joe Rothberg, January 1994
C
C - Input:
C   MMOD   / I  Signed global module index
C   IWAF   / I  Local wafer index
C
C - Output:
C   VJWAFM / I  = VDOK if successful
C               = VDERR if error occurred
C   JWAF   / I  Global wafer index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vgindx.h"
C
      INTEGER JWAF, MMOD, IWAF
C
C     Local variables
      INTEGER JMOD, JFAC, ILAY, IFAC, IMOD
C
C functions
      INTEGER VJMODM
C
C ----------------------------------------------------------------------
C
      IF ((MMOD.GE.-NSLOTS).AND.(MMOD.LE.NSLOTS).AND.(MMOD.NE.0)
     >    .AND. (IWAF.GE.1).AND.(IWAF.LE.NWAFEM)) THEN
C
C
        JMOD = VJMODM(MMOD)
        JFAC = IABS(MMOD)
        ILAY = JJLAYF(JFAC)
        IFAC = JIFACF(JFAC)
        IMOD = JIMODM(JMOD)
C
        JWAF = IJWAFR(ILAY,IFAC,IMOD,IWAF)
C
        VJWAFM = VDOK
C
      ELSE
C
        VJWAFM = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
