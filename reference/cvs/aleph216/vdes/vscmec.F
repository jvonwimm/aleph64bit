      INTEGER FUNCTION VSCMEC (IVIEW,ISCM1,NSCH,ISCMF,ISCML,IECOR)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  First and last strip-channel-in-module with readout direction
C - Steve Wasserbaech, 4 November 1994
C
C   Calculate the first and last strip-channel-in-module numbers
C   in a cluster, interchanging the numbers if necessary to take
C   the readout direction into account.  Thus a loop from ISCMF to
C   ISCML (stepping by IECOR) will be a loop in the order of
C   increasing *electronics* channel number.
C
C   Warning: this function does not check that ISCM1 and ISCM1+NSCH-1
C   fall within the same readout module.  If they do, then ISCMF is
C   the strip-channel-in-module number of the strip channel with the
C   lowest *electronics* channel number in the cluster and ISCML is
C   the strip-channel-in-module number of the strip channel with the
C   highest *electronics* channel number in the cluster.
C
C   If ((IVIEW is valid) .and. (NSCH .gt. 0)) then
C     If ((ISCM1 is a valid strip-channel-in-module) .and.
C         (ISCM1+NSCH-1 is a valid strip-channel-in-module)) then
C       IECOR = readout sign
C       If (readout sign = +1) then
C         ISCMF = ISCM1
C         ISCML = ISCM1+NSCH-1
C       Else
C         ISCMF = ISCM1+NSCH-1
C         ISCML = ISCM1
C       Endif
C     Endif
C   Endif
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   ISCM1  / I  First strip-channel-in-module number in cluster
C   NSCH   / I  Number of channels in cluster; must be greater than zero
C
C - Output:
C   VSCMEC / I  = VDOK if successful
C               = VDERR if error occurred
C   ISCMF  / I  = ISCM1        if readout direction = +1;
C               = ISCM1+NSCH-1 if readout direction = -1
C   ISCML  / I  = ISCM1+NSCH-1 if readout direction = +1;
C               = ISCM1        if readout direction = -1
C   IECOR  / I  Readout direction for this view
C               = +1 if strip channels and electronics channels are
C                 numbered in the same direction;
C               = -1 if in opposite directions.
C               Note: IECOR can be used as the step size to loop from
C               ISCMF to ISCML.
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vrdoco.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IVIEW, ISCM1, NSCH, ISCMF, ISCML, IECOR
C
C     Local variables
      INTEGER ISCMN, NSMOD
C
C ----------------------------------------------------------------------
C
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW .NE. VVIEWP)) THEN
C
C     invalid view:
        ISCMF = 0
        ISCML = 0
        IECOR = 0
        VSCMEC = VDERR
C
      ELSEIF (NSCH .LE. 0) THEN
C
C     invalid number of channels:
        ISCMF = 0
        ISCML = 0
        IECOR = 0
        VSCMEC = VDERR
C
      ELSE
C
C     The last strip-channel-in-module in the cluster:
        ISCMN = ISCM1 + NSCH - 1
C
C     Get IECOR and check the validity of ISCM1 and ISCMN:
C
        IF (IVIEW .EQ. VVIEWZ) THEN
          NSMOD = NZSMOD
          IECOR = IECORZ
        ELSE
          NSMOD = NPSMOD
          IECOR = IECORP
        ENDIF
C
        IF ((ISCM1 .LT. 1) .OR. (ISCMN .GT. NSMOD)) THEN
          ISCMF = 0
          ISCML = 0
          IECOR = 0
          VSCMEC = VDERR
C
        ELSE
C
          IF (IECOR .EQ. +1) THEN
            ISCMF = ISCM1
            ISCML = ISCMN
          ELSE
            ISCMF = ISCMN
            ISCML = ISCM1
          ENDIF
C
          VSCMEC = VDOK
C
        ENDIF
C
      ENDIF
C
      RETURN
      END
#endif
