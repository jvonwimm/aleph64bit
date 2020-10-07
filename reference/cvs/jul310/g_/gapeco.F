      SUBROUTINE GAPECO(IER)
C----------------------------------------------------------------------
C! - Build PECO bank NR=1 for modified Energy,Vertex
C!
C!   Author   :- MN Minard             27-JAN-1993
C!   Modified :- P.Janot               24-SEP-1996
C!               Create PECO NR=1 also when PECO NR=0 has zero length
C!
C!   IER = 1 Not enough space to built PECO
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "pecojj.h"
      DIMENSION XGER(3)
      DATA NAPECO / 0 /
#include "alcons.h"
#include "bmacro.h"
      IF(NAPECO.EQ.0) THEN
        NAPECO = NAMIND('PECO')
      ENDIF
      KPECO = IW(NAPECO)
      NPECO = -1
      IF ( KPECO.NE.0) NPECO = LROWS(KPECO)
C
C-    Define vertex
C
      X = 0.
      Y = 0.
      Z = 0.
      CALL EVTVER(X,Y,Z)
C
C-   Lift bank NR=1 for corrected vertex cluster
C
      IF (NPECO.GE.0) THEN
        LENG = LMHLEN + NPECO*LPECOA
        KPECO2 = NDROP('PECO',1)
        CALL AUBOS('PECO',1,LENG,KPECO2,IGARB)
        IF ( IGARB.EQ.2) GO TO 998
        IF ( IGARB.NE.0 ) THEN
          KPECO = IW(NAPECO)
        ENDIF
        IW(KPECO2+LMHLEN) = IW(KPECO+LMHLEN)
        IW(KPECO2+1) = IW(KPECO+1)
        DO IPECO = 1,NPECO
          JPECO = KROW(KPECO,IPECO)
          JPECO2 = KROW(KPECO2,IPECO)
          CALL UCOPY(RW(JPECO+1),RW(JPECO2+1),LPECOA)
          CALL GVERCR(IPECO,RAD,IER)
          IF ( IER.EQ.0) THEN
            XPHI = RW(JPECO+JPECPH)
            XTET = RW(JPECO+JPECTH)
            XGER(1) = RAD * COS(XPHI)* SIN(XTET) - X
            XGER(2) = RAD * SIN(XPHI)* SIN(XTET) - Y
            XGER(3) = RAD * COS(XTET) - Z
            DXER = SQRT(XGER(1)**2+XGER(2)**2)
            IF ( DXER.GT.0) THEN
            XPHIN = ATAN2(XGER(2),XGER(1))
            IF ( XPHIN.LT.0 ) XPHIN = XPHIN + TWOPI
            RW(JPECO2+JPECPH) = XPHIN
            RW(JPECO2+JPECTH) = ATAN2(DXER,XGER(3))
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      GOTO 999
C
  998 IER = 1
  999 RETURN
      END
#endif
