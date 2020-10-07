      INTEGER FUNCTION VRSWMX (IROS,IWAF,IVIEW,IDATC,IMX)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the MX7 chip number and data channel
C!                    for a given strip, wafer, view. VDET95
C - Joe Rothberg, August 1995
C
C - Input:
C   IROS   / I  Readout strip number
C   IWAF   / I  Local wafer index
C   IVIEW  / I  View
C
C - Output:
C   IMX   / I  MX7 chip number
C   IDATC / I  Data channel number
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C ----------------------------------------------------------------------
#include "vglobl.h"
#include "vsltco.h"
#include "vduefc.h"
C ----------------------------------------------------------------------
C
C     Arguments:
      INTEGER IROS, IWAF, IVIEW, IDATC, IMX
C
C     Local variables
      INTEGER IRET, IROM, ISCH, IECH, IMOD
C
C     External references:
      INTEGER VDRSSC, VDSCEC
C
C ----------------------------------------------------------------------
C
      VRSWMX = VDERR
C
C readout strip to strip channel
             IMOD = 1
             IRET = VDRSSC(IMOD,IWAF,IVIEW,IROS,IROM,ISCH)
             IF( IRET .NE. VDERR) THEN
C strip channel to electronics channel
                IRET = VDSCEC(IVIEW,ISCH, IECH)
                  IF(IRET .NE. VDERR ) THEN

C electronics channels to data channels
                     IDATC = IECH + VUECH(IVIEW) +VEXCH(IVIEW)
C data channels to MX chips
                     IMX    = (IDATC-VEXCH(IVIEW))/128 + 1
                     VRSWMX = VDOK
                  ENDIF
            ENDIF
C
      RETURN
      END
#endif
