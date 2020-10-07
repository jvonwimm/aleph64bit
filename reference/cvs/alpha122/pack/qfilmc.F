      SUBROUTINE QFILMC
CKEY FILL MC /INTERNAL
C-----------------------------------------------------------------------
C! Fill P vector with MC-TRUTH info
C!                                                   J.Hilgart 21.09.88
C!                                                  H.Albrecht 18.10.88
C!called from QFILL
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE INIT
#include "qcdesh.h"
      DATA INIT /0/
C-----------------------------------------------------------------------
C
      IF (IW(NAFKIN) .EQ. 0 .OR. IW(NAFVER) .EQ. 0) THEN
        XMCEV = .FALSE.
C
C         Do we have the old-fashioned KINE and VERT banks?
C
        IF (IW(NAKINE) .EQ. 0 .OR. IW(NAVERT) .EQ. 0)  GO TO 90
C
C         1-time initialization
C         convert KINE to FKIN with dropping tracks in the calorimeter
C
        IF (INIT .EQ. 0) THEN
          CALL FYIRUN (0.,.FALSE.,.TRUE.,0.)
          INIT = 1
        ENDIF
C
C         Convert them to the standard banks
C
        CALL FYKINE ('DROP','NO_GARB',IBFUL)
        IF (IBFUL.NE.0)  CALL QMTERM
     &    ('_QFILMC_ Not enough space to fill bank FKIN')
      ENDIF
      XMCEV = .TRUE.
C
C       Vertices
C
      CALL QFMCVX
C
C       MC particles. Fill daughter -> mother relation
C
      CALL QFMCPA
C
C       fill mother -> daughter relation
C
      CALL QFMCMD
C
C       bit masks
C
      CALL QFMCBM
C
C       stability code
C
      CALL QFMCST
C
C       fill match information
C
      CALL QFMTCH
C
   90 END
#endif
