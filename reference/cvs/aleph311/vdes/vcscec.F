      INTEGER FUNCTION VCSCEC (IVIEW,ISCH1,NSCH,IECH1)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Convert first strip channel in cluster to first electronics channel
C - Steve Wasserbaech, 4 November 1994
C
C   Convert first strip channel in cluster to first electronics channel.
C   This function takes the readout direction into account, i.e., the
C   strip channels and electronics channels may be numbered in the same
C   or in opposite directions.  The output of this function, IECH1,
C   is the lowest electronics channel number in the cluster.  It may
C   correspond to the first or the last strip channel in the cluster.
C
C   If ((IVIEW is valid) .and. (NSCH .gt. 0)) then
C     If ((ISCH1 is a valid strip channel) .and.
C         (ISCH1+NSCH-1 is a valid strip channel)) then
C       If (readout sign = +1) then
C         convert strip channel ISCH1 to electronics channel
C       Else
C         convert strip channel ISCH1+NSCH-1 to electronics channel
C       Endif
C     Endif
C   Endif
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   ISCH1  / I  First strip channel number in cluster
C   NSCH   / I  Number of channels in cluster; must be greater than zero
C
C - Output:
C   VCSCEC / I  = VDOK if successful
C               = VDERR if error occurred
C   IECH1  / I  First electronics channel number in cluster;
C               this corresponds to the *last* strip channel in the
C               cluster if the readout direction is -1.
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vrdoco.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IVIEW, ISCH1, NSCH, IECH1
C
C     Local variables
      INTEGER ISCHL, NSROM, IECOR, IRET
C
C     External references:
      INTEGER VDSCEC
C
C ----------------------------------------------------------------------
C
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW .NE. VVIEWP)) THEN
C
C     invalid view:
        IECH1 = 0
        VCSCEC = VDERR
C
      ELSEIF (NSCH .LE. 0) THEN
C
C     invalid number of channels:
        IECH1 = 0
        VCSCEC = VDERR
C
      ELSE
C
C     The last strip channel in the cluster:
        ISCHL = ISCH1 + NSCH - 1
C
C     Check the validity of ISCH1 and ISCHL:
C
        IF (IVIEW .EQ. VVIEWZ) THEN
          NSROM = NZSROM
          IECOR = IECORZ
        ELSE
          NSROM = NPSROM
          IECOR = IECORP
        ENDIF
C
        IF ((ISCH1 .LT. 1) .OR. (ISCHL .GT. NSROM)) THEN
          IECH1 = 0
          VCSCEC = VDERR
C
        ELSE
C
          IF (IECOR .EQ. +1) THEN
            IRET = VDSCEC(IVIEW,ISCH1,IECH1)
          ELSE
            IRET = VDSCEC(IVIEW,ISCHL,IECH1)
          ENDIF
C
          VCSCEC = VDOK
C
        ENDIF
C
      ENDIF
C
      RETURN
      END
#endif
