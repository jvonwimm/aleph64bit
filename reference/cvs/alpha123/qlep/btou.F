      LOGICAL FUNCTION BTOU(IND)
CKEY  QTRUTH/INTERNAL
C ...............................................................
C! Finds b -> u decays in QTRUTH
C  Authors  D. Abbaneo , F. Ligabue 27/07/94  from FINLEP / UPHY
C  Called from GEN in QTRUTH package
C . True if b->u decay.                                         .
C .   IND    / I   Alpha index of the true track which is       .
C .                daughter of the b particle                   .
C ...............................................................
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
C
C - functions
      LOGICAL FCHAD,FBHAD,FCQUA,FBQUA,FCBAR,FBBAR,CCBMES
C
C - variables
      INTEGER IND,JDAU,J,MOM,JK,JNEP
C
C - macros
#include "qmacro.h"
C---------------------------------------------------------------------
C - "normal" decays
      BTOU=.TRUE.
      MOM = KMOTH(IND,1)
      DO J=1,KNDAU(MOM)
        JDAU=KDAU(MOM,J)
C
C - b decay to c-cbar mesons
        IF (CCBMES(JDAU)) BTOU=.FALSE.
        IF (JDAU.NE.IND.AND.FCHAD(JDAU)) BTOU=.FALSE.
C
C - handle case of b -> b gamma
        IF (FBHAD(JDAU)) THEN
18        DO JK=1,KNDAU(JDAU)
            JNEP=KDAU(JDAU,JK)
            IF (FBQUA(JDAU).AND.FCQUA(JNEP)) BTOU=.FALSE.
            IF (FBBAR(JDAU).AND.FCBAR(JNEP)) BTOU=.FALSE.
            IF (CCBMES(JNEP)) BTOU=.FALSE.
            IF (FBHAD(JNEP)) THEN
              JDAU=JNEP
              GOTO 18
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      RETURN
      END
#endif
