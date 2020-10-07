      SUBROUTINE TPCBUN(ITPBUN,IQUAL)
C-----------------------------------------------------------------------
C! Determine the TPC bunch number and set the quality flag
C!
C!    Author M.Girone, I. Tomalin 21/3/95
C!
C! Input arguements:
C!   None (but uses TGFT bank via TPCT0N).
C!
C! Output arguments:
C!   INTE ITPBUN: TPC bunch number.
C!   INTE IQUAL : Flag indicating reliability of ITPBUN.
C!                (=0 => No information, =1 probably correct,
C!             =2 => almost certainly correct, =3 => certainly correct).
C!
C! Calls: RQBUNC, TPCT0N
C?
C!======================================================================
#ifndef DOC
      DIMENSION NTR(2)
C Define some uncertainty on t0 to allow for tracking distortions,
C error on drift velocity etc.
      PARAMETER(EXTRAE=0.003)
C-----------------------------------------------------------------------
      IQUAL = 0
C
C Find current ECAL/ITC bunch number and interbunch spacing (ns).
      CALL RQBUNC(IBUN,INBU,NWAG,IQUA)
C
      IF (NWAG.GT.1.AND.INBU.GT.0) THEN
C
C Find TPC Delta(T0) in microseconds and its error.
        CALL TPCT0N(IER,DT,EDT,NTR)
        DT  = 0.5*DT
        EDT = 0.5*EDT
C Increase error to allow for tracking distortions etc.
        EDT = SQRT(EDT**2 + EXTRAE**2)
C
C Fill histograms (and output good events to disk).
        IF (IER.EQ.0.AND.ABS(DT).LT.999.9) THEN

C Get the TPC offset relative to ITC/ECAL in units of bunch number.
          TPOFF = -DT*1000./FLOAT(INBU)
          ITPOFF = NINT(TPOFF)
C Get the TPC Bunch number and its error. (N.B. If RQBUNC did not know
C bunch number, TPREDA assumed it was 1.)
          IF (IBUN.GE.1) THEN
            TPBUN = IBUN + TPOFF
          ELSE
            TPBUN = 1 + TPOFF
          END IF
          ITPBUN = NINT(TPBUN)
          ITPBUN = MAX(ITPBUN,1)
          ITPBUN = MIN(ITPBUN,NWAG)
          ETPBUN = EDT*1000./FLOAT(INBU)
C
C Note number of standard deviations from most likely and also
C next most likely bunch.
          CHBEST = ABS(TPBUN - ITPBUN)/ETPBUN
C
          CHNEXT = 9.9E30
          DO 25 J = 1,NWAG
            CHIDIF = ABS(TPBUN - FLOAT(J))/ETPBUN
            IF (J.NE.ITPBUN.AND.CHIDIF.LT.CHNEXT) CHNEXT = CHIDIF
   25     CONTINUE
C
C Estimate the TPC bunch number quality (high number means good).
          CHIDIF = SQRT(MAX(CHNEXT**2 - CHBEST**2,0.0))
          IF (CHIDIF.GT.6.0) THEN
            IQUAL = 3
          ELSE IF (CHIDIF.GT.4.0) THEN
            IQUAL = 2
          ELSE IF (CHIDIF.GT.2.0) THEN
            IQUAL = 1
          END IF
C Check consistency with most likely bunch.
          IF (CHBEST.GT.5.0) THEN
            IQUAL = MAX(IQUAL-2,0)
          ELSE IF (CHBEST.GT.3.0) THEN
            IQUAL = MAX(IQUAL-1,0)
          END IF
C Be suspicious if not many tracks were used.
          IF (MIN(NTR(1),NTR(2)).EQ.1) THEN
            IQUAL = MAX(IQUAL-2,0)
          ELSE IF (MIN(NTR(1),NTR(2)).EQ.2) THEN
            IQUAL = MAX(IQUAL-1,0)
          END IF
        END IF
C
      ELSE
C No bunch train.
        ITPBUN = 1
        IQUAL = 3
      END IF
C
      RETURN
      END
#endif
