      INTEGER FUNCTION VDACRS (IDATC,IVIEW,IWAF,IROS)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the strip, wafer for given data channel and view.
C!      returns one wafer and readout strip even if multiplexed. VDET95
C - Joe Rothberg, August 1995
C
C - Input:
C   IDATC  / I  Data Channel number
C   IVIEW  / I  View
C
C - Output:
C   IWAF   / I  Local wafer index
C   IROS   / I  Readout Strip number
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C ----------------------------------------------------------------------
#include "vglobl.h"
#include "vsltco.h"
#include "vduefc.h"
C ----------------------------------------------------------------------
C     Arguments:
      INTEGER IROS, IWAF, IVIEW, IDATC
C
C     Local variables
      INTEGER IRET, IROM, ISCH, IECH, IMOD
C
C     External references:
      INTEGER VDSCRS, VDECSC
C ----------------------------------------------------------------------
C
      VDACRS = VDERR
C
C Data channel to electronics channel
C       both start from zero
      IF(IVIEW .EQ. 1 .OR. IVIEW .EQ. 2) THEN
         IECH  = IDATC - VUECH(IVIEW) -VEXCH(IVIEW)
C Electronics channel to strip channel
C   vdecsc checks for negative electronics channels
         IRET = VDECSC(IVIEW,IECH,ISCH)
         IF(IRET .EQ. VDOK) THEN
C strip channel to Readout Strip
            IROM = 1
            IRET = VDSCRS(IVIEW,IROM,ISCH,IMOD,IWAF,IROS)
            IF(IRET .EQ. VDOK) THEN
               VDACRS = VDOK
            ENDIF
         ENDIF
      ENDIF
C
      RETURN
      END
#endif
