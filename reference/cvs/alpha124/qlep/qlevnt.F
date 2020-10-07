CKEY  QSELEP/USER
      SUBROUTINE QLEVNT
C----------------------------------------------------------------------
C! Steering of Heavy Flavour Lepton selection
C Called from QFILL
C                                                   M.N Minard 08.11.94
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "rhahjj.h"
      SAVE XBANK,NAQLID
      EXTERNAL CHAINT,NAMIND
      CHARACTER*4 CHAINT
      CHARACTER*8 PRNAM
      LOGICAL XBANK
      DATA NAQLID / 0 /
#include "qmacro.h"
C----------------------------------------------------------------------
      IF (XNANO) GO TO 999
      IF ( NAQLID.EQ.0) THEN
         XBANK=.FALSE.
         NAQLID = NAMIND('QLID')
         IF ( IW(NAQLID).NE.0) THEN
            XBANK = .TRUE.
            CALL BLIST(IW,'C+','PLSC')
            CALL BLIST(IW,'R+','PLSC')
         ENDIF
      ENDIF
      IF ( .NOT.XBANK) GO TO 999
C
C-    Now call lepton ID  routine
C
      ILOUT = KUPRNT
      CALL QSELEP(ILOUT,XBANK,NLEP,IERR)
      IF (IERR.GE.0) CALL QFLEPT
C
 999  RETURN
      END
#endif
