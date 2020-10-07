      SUBROUTINE GETNAV(NAVERS)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C!  - Returns the NANO version used for reconstruction
C!
C!   Author   :- Gerrit Graefe       27-FEB-1995
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - NVERS /I    : NANO version
C!
C!   Libraries required: BOS77
C!
C!   Description
C!   ===========
C!   Input bank : RHAH  ( Run Header Analysis History )
C?
C!======================================================================
#ifndef DOC
      SAVE FIRST,NVERS
#include "bcs.h"
#include "rhahjj.h"
      CHARACTER*4 CHAINT,NCVERS,PRNAM
      INTEGER NVERS,INDST,NAVERS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bmacro.h"
      IF(FIRST)THEN
C
C!..NANO VERSION
C
        NVERS=0
        IRHAH=IW(NAMIND('RHAH'))
        IF(IRHAH.EQ.0)THEN
          NVERS=-1
        ELSE
          DO 10 I=1,LROWS(IRHAH)
            KRHAH = KROW(IRHAH,I)
            PRNAM = CHAINT(IW(KRHAH+JRHAPN))
            IF(PRNAM.EQ.'NANO')THEN
              NVERS = IW(KRHAH+JRHAPV)
            ENDIF
   10     CONTINUE
        ENDIF
        FIRST=.FALSE.
      ENDIF
      NAVERS=NVERS
      RETURN
      END
#endif
