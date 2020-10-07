        SUBROUTINE KXP6TO(KLUN,TLIF)
C-----------------------------------------------------------------------
C January 12 1998 B. Bloch for Pythia 6.1
C! LUND application routine to find lifetime of non-standard particles
CKEY KINE KINGAL LUND7 LIFETIME /   INTERNAL
C - input   :  KLUN = LUND particle code
C - output  :  TLIF = particle lifetime ( in seconds )
C Useful only for particles not defined in the ALEPH database
C-----------------------------------------------------------------------
#ifndef DOC
        DATA       TCHAB ,          TBMES  ,         TBBAR
     + /           4.3E-13        , 1.5E-12    ,    1.22E-12      /
C         Charmed baryons          B   mesons     B   baryons
C
        TLIF=0.
        K=IABS(KLUN)
C B MESONS have code 5xx  or x05xx
C B BARYONS have code 5xxx
C C BARYONS have code 4xxx
C Diquarks  have code xx0x            :life time set to 0
C Excited states with 2s+1>= 3        :life time set to 0
C Higher meson multiplets with spin=1 :life time set to 0
        IBARY = MOD(K/1000,10)
        IMESO = MOD(K/100,10)
        IDIQU = MOD(K/10,10)
        IEXCI = MOD(K,10)
        IHIGH = MOD(K/10000,10)
        IF (IDIQU.EQ.0) GO TO 999
        IF (IEXCI.GE.3) GO TO 999
        IF (IEXCI.EQ.1 .AND. IHIGH.GT.0) GO TO 999
        IF ( IBARY.EQ.4) THEN
           TLIF = TCHAB
        ELSEIF ( IBARY.EQ.5) THEN
           TLIF = TBBAR
        ELSEIF ( IBARY.EQ.0 .AND. IMESO.EQ.5) THEN
           TLIF = TBMES
        ENDIF
C
 999    RETURN
        END
#endif
