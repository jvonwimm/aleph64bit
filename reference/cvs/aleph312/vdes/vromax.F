      INTEGER FUNCTION VROMAX (IVIEW,IROMX,INCR)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns limit and step size for looping over readout modules
C - Steve Wasserbaech, January 1995
C
C   To loop over all readout modules in a face/view, loop from 1 to
C   IROMX with step size INCR.  This handles the peculiar numbering
C   of the readout modules.  (If the view has multiplexing, then each
C   face has two readout modules numbered IROM=1 and IROMAX.)
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VROMAX / I  = VDOK if successful;
C               = VDERR if IVIEW is invalid.
C   IROMX  / I  Maximum readout module index in face IROM
C   INCR   / I  Step size for looping
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vzpwco.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IVIEW, IROMX, INCR
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
C
        IF (LZMULT) THEN
          IROMX = IROMAX
          INCR = IROMAX - 1
        ELSE
          IROMX = NWAFEF
          INCR = 1
        ENDIF
        VROMAX = VDOK
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        IROMX = IROMAX
        INCR = IROMAX - 1
        VROMAX = VDOK
C
      ELSE
C
C     invalid view:
        IROMX = 0
        INCR = 1
        VROMAX = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
