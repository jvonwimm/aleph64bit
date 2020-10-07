      SUBROUTINE LBLDBA
C-----------------------------------------------------------------------
C! Build background events from single arm trigger events.
C!
C!   Author   : P. H. Hansen   890810
C! Modified   : J. Wear        891109
C!              Allows background selection to be conveniently done at
C!              end-of-run (or any time).
C! Modified   : P. H. Hansen   960501   enable debug print
C!
C!   Input    : 'LSIN'          single arm storage bank
C!   Output   : 'LBAK','LONL'   Fake Bhabha event statistics
C!   Calls    : routine LSELBA
C!
C!   Description :
C!   ===========
C?   Loops over single rows of LSIN bank and passes combinations to
C?   background selection routine.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "lsinjj.h"
#include "lonljj.h"
#include "alcons.h"
      EXTERNAL NDROP
#include "bmacro.h"
C----------------------------------------------------------------------
C
      KLSIN = IW(NALSIN)
      IF(KLSIN.LE.0)                               GOTO 999
      NSIN = LROWS(KLSIN)
      KLONL = IW(NALONL)
      IF(KLONL.GT.0) THEN
        RW(KLONL+LMHLEN+JLONRA) = RW(KLONL+LMHLEN+JLONRA)+1.
        IF(FDEBRF.AND.JDBDRF(5).GE.2) THEN
          WRITE(IW(6),'(/,2X,A,E12.4,A,I5/)')
     &     '+++LBLDBA+++ Update background at GBX=',
     &      RW(KLONL+LMHLEN+JLONGB),'  #singles=',NSIN
        ENDIF
      ENDIF
      IF(NSIN.LE.1) THEN
         GOTO 999
      ENDIF
      DO 200 I=1,NSIN-1
        DO 100 J=I+1,NSIN
          IA = 0
          IB = 0
          IF(RTABL(KLSIN,I,JLSITH).LT.PIBY2) THEN
            IA = I
          ELSE
            IB = I
          ENDIF
          IF(RTABL(KLSIN,J,JLSITH).LT.PIBY2) THEN
            IA = J
          ELSE
            IB = J
          ENDIF
          IF(IA.EQ.0.OR.IB.EQ.0) GOTO 100
          CALL LSELBA(IA,IB)
  100   CONTINUE
  200 CONTINUE
      KLSIN = NDROP('LSIN',0)
  999 CONTINUE
      END
#endif
