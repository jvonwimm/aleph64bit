      SUBROUTINE GETJUV(JUVER)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C!  - Returns the JULIA version used for reconstruction
C!
C!   Author   :- Yves A. Maumary       17-JAN-1992
C!   Modified :- Yves A. Maumary       14-AUG-1992  Bug correction
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - JUVER /I    : JULIA version
C!
C!   Libraries required: BOS77
C!
C!   Description
C!   ===========
C!   Input bank : RHAH  ( Run Header Analysis History )
C?
C!======================================================================
#ifndef DOC
      SAVE FIRST,NAMRH
#include "bcs.h"
#include "rhahjj.h"
      CHARACTER*4 CHAINT,PRNAM
      INTEGER NAMRH,JRHAH
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST = .FALSE.
        NAMRH = NAMIND('RHAH')
      ENDIF
      JRHAH = IW(NAMRH)
      IF(JRHAH.EQ.0)THEN
        CALL ALTELL('_GETJUV_ no RHAH bank',0,'RETURN')
        JUVER = -1
        RETURN
      ENDIF
C
C!.LOOP OVER ALL ROWS TO BE SURE TO PICK UP THE LAST PROCESSING
      NROWS = LROWS(JRHAH)
      DO 10 I=1,NROWS
        KRHAH = KROW(JRHAH,I)
        PRNAM = CHAINT(IW(KRHAH+JRHAPN))
        IF(PRNAM.EQ.'JULI')THEN
          JUVER = IW(KRHAH+JRHAPV)
        ENDIF
   10 CONTINUE
      END
#endif
