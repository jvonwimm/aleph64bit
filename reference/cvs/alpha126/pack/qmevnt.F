      SUBROUTINE QMEVNT(IRUN,IEVT)
CKEY EVENT /INTERNAL
C----------------------------------------------------------------------
C! event processing
C! called from QMAIN
C!                                                  H.Albrecht 20.09.88
C!                                                  J.Boucrot  10.01.99
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C----------------------------------------------------------------------
C User routine for track refit, if necessary:
      CALL QUREFIT
C
C++   Fix some bank structure or content.
C++   This must be done if and only if RHAH is updated in QWRUNR.
C
      IF (.NOT.XCOPYJ .OR. XWMINI) THEN
        CALL FIXPECO
        CALL FIXPCRL
      ENDIF
C
      IF (XCOPYJ)  THEN
        CALL QWRITE
      ELSE
C++ Force the track extrapolation for muon-id if MEXT card exists
        JMEXT=IW(NAMIND('MEXT'))
C
        IF(.NOT.XMINI .AND. (JMEXT.GT.0 .OR.
     &  (IW(NAMUID).LE.0 .AND.
     &  (IW(NAHMAD).GT.0.OR.IW(NAMCAD).GT.0)))) CALL QUMUPR
C Fill internal ALPHA banks and variables:
        IF (XFILL) CALL QFILL
C Event processing for official MINI production :
        CALL MQUEVNT
C User's event processing:
        CALL QUEVNT (IW(KOQVEC+KCQVEC+1), IW(KOQVEC+KCQVEC+1),
     +               IW(KOQVRT+KCQVRT+1), IW(KOQVRT+KCQVRT+1))
      ENDIF
      END
#endif
