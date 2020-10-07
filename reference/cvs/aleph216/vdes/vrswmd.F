      INTEGER FUNCTION VRSWMD (IROS,IWAF,MMOD,IVIEW,IDATC)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the data channel for a given strip,wafer,module,view. VDET95
C!     this will contain bonding error information
C - Joe Rothberg, August 1995
C
C - Input:
C   IROS   / I  Readout strip number
C   IWAF   / I  Local wafer index
C   MMOD   / I  signed module number
C   IVIEW  / I  View
C
C - Output:
C   IDATC / I  Data channel number
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C ----------------------------------------------------------------------
#include "vglobl.h"
#include "vsltco.h"
#include "vduefc.h"
C ----------------------------------------------------------------------
C     Arguments:
      INTEGER IROS, IWAF, IVIEW, IDATC, MMOD
C
C     Local variables
      INTEGER IRET, IROM, ISCH, IECH, IMOD, JMOD, ILAY, IFAC
C
C     External references:
      INTEGER VDRSSC, VDSCEC, VJMODM, VIMODI
C
C ----------------------------------------------------------------------
C
      VRSWMD = VDERR
C
      IF ((MMOD.GE.-NSLOTS).AND.(MMOD.LE.NSLOTS).AND.(MMOD.NE.0)) THEN
             JMOD =  VJMODM(MMOD)
             IRET = VIMODI(JMOD,ILAY,IFAC,IMOD)
             IF(IRET .EQ. VDOK) THEN
C readout strip to strip channel
                IRET = VDRSSC(IMOD,IWAF,IVIEW,IROS,IROM,ISCH)
                IF( IRET .NE. VDERR) THEN
C strip channel to electronics channel
                   IRET = VDSCEC(IVIEW,ISCH, IECH)
                     IF(IRET .NE. VDERR ) THEN
C electronics channels to data channels
                        IDATC = IECH + VUECH(IVIEW) + VEXCH(IVIEW)
                        VRSWMD = VDOK
                     ENDIF
                ENDIF
             ENDIF
      ENDIF
C
      RETURN
      END
#endif
