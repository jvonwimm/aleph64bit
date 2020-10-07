      SUBROUTINE TSMFT
C!SMEAR AND refit frft  tracks IN MC EVENTS
CKEY TPC SMEARING
C
C  Author      : A. BONISSENT 050298
C
C----------------------------------------------------------------------
#ifndef DOC
      SAVE NATPCO, NATSMR, FIRST
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bcs.h"
#include "bmacro.h"
      IF(FIRST)THEN
        FIRST=.FALSE.
        NATPCO = NAMIND('TPCO')
        NATSMR = NAMIND('TSMR')
      ENDIF
      KTPCO = IW(NATPCO)
C
C check data card TSMR : -1 means no smearing
C
      KTSMR = IW(NATSMR)
      IF(KTSMR.NE.0)THEN
        IF(IW(KTSMR+1).EQ.-1)GO TO 999
      ENDIF
C Skip events without TPCO bank, nothing to do.
      IF(KTPCO.EQ.0)GO TO 999
C
C Save TPCO bank into TPCO 1000
C Skip all smearing if new banks cannot be created
C
      NDATA = IW(KTPCO)
      CALL AUBOS('TPCO',1000,NDATA,K1000,IGARB)
      IF(IGARB.EQ.2)GO TO 998
      KTPCO = IW(NATPCO)
      CALL UCOPY(IW(KTPCO+1),IW(K1000+1),NDATA)
C
C Copy FRFT 0 and 2 into 10 and 12
C
      DO NRFR = 0,2,2
        KFR0 = NLINK('FRFT',NRFR)
        IF(KFR0.NE.0)THEN
          NDATA = IW(KFR0)
          CALL AUBOS('FRFT',10+NRFR,NDATA,KFR10,IGARB)
          IF(IGARB.EQ.2)GO TO 998
          KFR0 = NLINK('FRFT',NRFR)
          CALL UCOPY(IW(KFR0+1),IW(KFR10+1),NDATA)
        ENDIF
      ENDDO                 
C
      CALL TPHSMR
      CALL TREFIT
C
C Restore original TPCO bank
C
      KTPCO = IW(NATPCO)
      K1000 = NLINK('TPCO',1000)
      NDATA = IW(K1000)
      CALL UCOPY(IW(K1000+1),IW(KTPCO+1),NDATA)
      CALL NDROP('TPCO',1000)
  999 CONTINUE
      RETURN
  998 CONTINUE
C 
C Cleanup in case of BOS space insufficient
C
      CALL RERROR('TSMFT',1,'Insufficient BOS space, skip TPC smearing')
      CALL NDROP('TPCO',1000)
      CALL NDROP('FRFT',10)
      CALL NDROP('FRFT',12)
      RETURN
      END
#endif
