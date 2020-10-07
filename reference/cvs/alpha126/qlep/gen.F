      SUBROUTINE GEN(MTRA,IPROC,IBMES)
CKEY  QTRUTH/INTERNAL
C ...............................................................
C! Determines the production process and decaying b in QTRUTH
C  Authors  D. Abbaneo , F. Ligabue 27/07/94  from FINLEP / UPHY
C  Called from QTRUTH
C .                                                             .
C . Input                                                       .
C .   MTRA   / I   Alpha index of the MC track which is the     .
C .                daughter of the string or of the last        .
C .                H.F. hadron                                  .
C .                                                             .
C . Output                                                      .
C .   IPROC  / I   Production process                           .
C .                 1    b -> c X                               .
C .                 2    b -> u X                               .
C .                 3    b -> cbar c ; cbar -> X                .
C .                 4    b -> cbar u ; cbar -> X                .
C .                 5    b -> c -> X                            .
C .                 6    c -> X                                 .
C .                 7    fragmentation or uds                   .
C .                                                             .
C .   IBMES  / I   Identity of the decaying b particle (if any) .
C .                -2   "oscillated" Bs                         .
C .                -1   "oscillated" Bd                         .
C .                 0   b baryon or non b particle              .
C .                 1   Bd                                      .
C .                 2   Bs                                      .
C .                 3   B+                                      .
C .                                                             .
C ...............................................................
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
C
C - functions
      LOGICAL FHVFL,BTOU,FBHAD,FBQUA,FBBAR,FCQUA,FCBAR,ENDCH
C
C - variables
      INTEGER MTRA,IPROC,NON,IBMES,IBFUNC,MGM
C
C - macros
#include "qmacro.h"
C----------------------------------------------------------------------
      IBMES=0
C
C - fragmentation or uds
      IPROC = 7
      NON = KMOTH(MTRA,1)
      IF (.NOT.FHVFL(NON)) RETURN
      IF (FBHAD(NON)) THEN
C
C - b->l
        IPROC=1
C
C - charmless b->l
        IF (BTOU(MTRA)) IPROC=2
        IBMES=IBFUNC(NON)
      ELSE
89      MGM=KMOTH(NON,1)
        IF (ENDCH(MGM)) THEN
C
C - c->l
          IPROC=6
        ELSEIF(FBHAD(MGM)) THEN
          IBMES=IBFUNC(MGM)
          IF ((FBQUA(MGM).AND.FCQUA(NON)).OR.
     @       (FBBAR(MGM).AND.FCBAR(NON))) THEN
C
C - b->c->l
            IPROC=5
          ELSE
C
C - b->cbar c
            IPROC=3
C
C - b->cbar u
            IF (BTOU(NON)) IPROC=4
          ENDIF
        ELSE
          NON=MGM
          GOTO 89
        ENDIF
      ENDIF
      RETURN
      END
#endif
