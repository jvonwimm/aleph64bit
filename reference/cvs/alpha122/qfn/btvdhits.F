      SUBROUTINE BTVDHITS(HITPAT)
CKEY   QIPBTAG / INTERNAL
C ------------------------------------------------------------------------------
C!  Simple subroutine to count the number of VDET hits by layer and view.
C  Called from TRSEL or QFNDV0
C  Input: none
C          calls VDHITS if POT or DST as input file .
C          else take information from bank DTRA
C
C  Output: HITPAT, an integer array numbered by FRFT track number,
C          of which only the first 8 bits are used, as described below
C          bits:          meaning
C          0-1            number of inner layer U hits on track (0,1,or2)
C          2-3            number of outer layer U hits on track     "
C          4-5            number of inner layer W hits on track     "
C          6-7            number of outer layer W hits on track     "
C          This array must have size at least as large as the number of
C          entries in FRFT; NO CHECK CAN BE MADE HERE, if this isn't
C          the case, the code will bomb.
C
C  created : 24-4-92 M.Frank : READ DTRA bank on mini-dst !
C  Modified  21-10-93 D. Brown, look at mini version to
C                                          decide which word to use
C ------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  Global includes
C
#include "qdecl.h"
#include "qcde.h"
#include "maxtrk.h"
C
C  Output; hit pattern array
C
      INTEGER HITPAT(*)
C
C  Functions
C
      INTEGER NAMIND,MINGTV
C
C  Local variables
C
      INTEGER NDTRA,IDTRA
      INTEGER NTRK,ITRK
      INTEGER MVER,JVI,DUM
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "qmacro.h"
C ------------------------------------------------------------------------------
C  Initialize the name indices the first time through
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        NDTRA = NAMIND('DTRA')
        IF(XMINI)THEN
          MVER = MINGTV(DUM)
          WRITE (IW(6),*)'QIPBTAG- VDET hits from mini version ',MVER
        END IF
      END IF
C--   mini-dst : bank DTRA
      IF ( XMINI ) THEN
c
c  VDET pattern word position changes as a function of version!!!
c
        MVER = MINGTV(DUM)
        IF (MVER.GE.70) JVI = 34
        IF (MVER.GE.90) JVI = 27
C
C     Loop over the hits
C
        IDTRA=IW(NDTRA)
        NTRK = LROWS(IDTRA)
        DO 500 ITRK = 1 , NTRK
          IF (ITRK.GT.MAXTRK) GO TO 500
          HITPAT(ITRK)=IAND(ITABL(IDTRA,ITRK,JVI),255)
  500   CONTINUE
        RETURN
      ELSE
        CALL VDHITS(HITPAT,MAXTRK)
      END IF
      RETURN
      END
#endif
