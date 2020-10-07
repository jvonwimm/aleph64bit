      SUBROUTINE TPELSG(POISAV,XLENG,NEL)
C
C! Fast sim : Routine to determine the number of primary clusters in a
C! super-broken segment.
C
C  Called from: T2DEDX
C  Calls:       RNDM,RANNOR
C
C  Inputs:    PASSED:  POISAV, Poisson average for number of primary
C                              ionizing interactions per unit length
C                      XLENG,  length of this track segment
C
C  Outputs:   PASSED:  NEL,    number of electrons in this segment.
C
C  P. Janot  11/15/87.
C
C  The probability of NEL primary ionizations in a length XLENG
C  can be easily determined :
C
C   p(Nel,Xleng) = (1/Nel!)*(Xleng*Poisav)**Nel*exp(Xleng*Poisav)
C
      DOUBLE PRECISION XPN,FACTI,XPROB
      FACTI  = 1.
      XOVSIG = XLENG*POISAV
      IF(XOVSIG.GT.80.) GOTO 100
      EXSIG  = EXP(-XOVSIG)
      XPN    = EXSIG
C
      XPROB  = RNDM(NEL*POISAV)
C
C     WRITE(6,1000) XOVSIG,XPROB,XPN
 1000 FORMAT(1X,'XOVSIG,XPROB,XPN : ',3E14.4)
C
      DO 1 INEL=1,100
        IF(XPROB.LT.XPN) THEN
           NEL   = INEL-1
           GOTO 200
        ELSE
           FACTI  = FACTI/FLOAT(INEL)*XOVSIG
           XPN   = XPN + FACTI*EXSIG
        ENDIF
  1   CONTINUE
C
  100 CONTINUE
      CALL RANNOR(ANEL,DUMMY)
      NEL   = XOVSIG*(1.+ANEL/SQRT(XOVSIG)) +.5
C
  200 CONTINUE
C
      IF(NEL.LT.0) NEL=1
      RETURN
      END
