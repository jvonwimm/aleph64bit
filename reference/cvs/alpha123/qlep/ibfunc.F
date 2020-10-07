      INTEGER FUNCTION IBFUNC(IND)
CKEY  QTRUTH/INTERNAL
C ...............................................................
C! Determines the identity of decaying b in QTRUTH
C  Authors  D. Abbaneo , F. Ligabue 27/07/94  from FINLEP / UPHY
C  Called from GEN in QTRUTH package
C . Gives the identity of the decaying b particle               .
C .                -2   "oscillated" Bs                         .
C .                -1   "oscillated" Bd                         .
C .                 0   b baryon or non b particle              .
C .                 1   Bd                                      .
C .                 2   Bs                                      .
C .                 3   B+                                      .
C ...............................................................
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
C
C - functions
      LOGICAL FBHAD,FBQUA,FBBAR
C
C - variables
      INTEGER IND,MOM,MGM
C
C - macros
#include "qmacro.h"
C---------------------------------------------------------------------
C - start
      IBFUNC=0
      IF (XPEQOR(IND,'B+')) THEN
C
C - charged meson
        IBFUNC=3
        RETURN
      ELSEIF (XPEQOR(IND,'B0').OR.XPEQOR(IND,'Bs0')) THEN
C
C - neutral mesons
        IBFUNC=1
C
C - go back to first beauty hadron
        MOM=IND
89      MGM=KMOTH(MOM,1)
        IF (FBHAD(MGM)) THEN
          MOM=MGM
          GOTO 89
        ENDIF
C
C - check for B meson mixing
        IF (FBQUA(IND).AND.FBBAR(MOM)) IBFUNC=-IBFUNC
        IF (FBBAR(IND).AND.FBQUA(MOM)) IBFUNC=-IBFUNC
C
C - distinguish Bs meson
        IF (XPEQOR(IND,'Bs0')) IBFUNC=IBFUNC*2
      ENDIF
      RETURN
      END
#endif
