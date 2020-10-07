      INTEGER FUNCTION VSCMSC (MMOD,IVIEW,ISCM,IROM,ISCH)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Convert strip-channel-in-module number to strip channel number
C - Steve Wasserbaech, 4 November 1994
C
C - Input:
C   MMOD   / I  Signed global module index
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   ISCM   / I  Strip-channel-in-module number
C
C - Output:
C   VSCMSC / I  = VDOK if successful
C               = VDERR if error occurred
C   IROM   / I  Readout module
C   ISCH   / I  Strip channel number
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vzpwco.h"
#include "vgindx.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER MMOD, IVIEW, ISCM, IROM, ISCH
C
C-----------------------------------------------------------------------
C
C     Check the validity of MMOD:
      IF ((MMOD .EQ. 0) .OR. (IABS(MMOD) .GT. NSLOTS)) THEN
        VSCMSC = VDERR
C
      ELSEIF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
C
C     Check the validity of the strip-channel-in-module number:
C
        IF ((ISCM .LT. 1) .OR. (ISCM .GT. NZSMOD)) THEN
          VSCMSC = VDERR
C
        ELSE
C
          IF (MMOD .LT. 0) THEN
C
C     This is the module at z < 0:
C
            IF (NZROMM .EQ. 1) THEN
              IROM = 1
            ELSE
              IROM = NWAFEM - (ISCM-1)/NZSROM
            ENDIF
C
          ELSE
C
C     This is the module at z > 0:
C
            IF (NZROMM .EQ. 1) THEN
              IF (LZMULT) THEN
                IROM = IROMAX
              ELSE
                IROM = NWAFEF
              ENDIF
            ELSE
              IROM = IROMAX - NWAFEM + 1 + (ISCM-1)/NZSROM
            ENDIF
C
          ENDIF
C
          ISCH = MOD(ISCM-1,NZSROM) + 1
          VSCMSC = VDOK
C
        ENDIF
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
C
C     Check the validity of the strip-channel-in-module number:
C
        IF ((ISCM .LT. 1) .OR. (ISCM .GT. NPSMOD)) THEN
          VSCMSC = VDERR
C
        ELSE
C
          IF (MMOD .LT. 0) THEN
C
C     This is the module at z < 0:
C
            IROM = 1
C
          ELSE
C
C     This is the module at z > 0:
C
            IROM = IROMAX
C
          ENDIF
C
          ISCH = ISCM
          VSCMSC = VDOK
C
        ENDIF
C
      ELSE
C
C     Invalid view:
C
        VSCMSC = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
