        SUBROUTINE KXLUTO(KLUN,TLIF)
C-----------------------------------------------------------------------
C A. Blondel  24 - 06 - 86
C Modified june 15, 1988   B. Bloch for updated B life times
C! LUND application routine to find lifetime of non-standard particles
CKEY KINE KINGAL LUND LIFETIME /   INTERNAL
C - input   :  KLUN = LUND particle code
C - output  :  TLIF = particle lifetime ( in seconds )
C Useful only for particles not defined in the ALEPH database
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
        DATA       TCHAB ,          TBMES  ,         TBBAR
     + /           4.3E-13        , 1.3E-12    ,    1.3E-12      /
C         Charmed baryons          B   mesons     B   baryons
C
        TLIF=0.
        K=IABS(KLUN)
      IF (K.EQ.8 .OR. K.EQ.10 .OR. K.EQ.12) TLIF = 1.E+15
        IF (K.LE.58.OR.K.GE.308) GO TO 999
        IF (K.EQ.59.OR.K.EQ.60.OR.K.EQ.80) TLIF=TCHAB
        IF (K.GE.101.AND.K.LE.104) TLIF=TBMES
        IF (K.GE.145.AND.K.LE.158) TLIF=TBBAR
        IF (K.GE.241.AND.K.LE.246) TLIF=TBBAR
        IF (K.GE.293.AND.K.LE.307) TLIF=TBBAR
C
 999    RETURN
        END
#endif
