*DK caabsb
      SUBROUTINE CAABSB(EMGNRJ,EMBETA,EMALFA,TINOX0,IER)
C ---------------------------------------------------------------
C - M.N.Minard - 930901
CKEY CALO
C! TEST ABSORBPTION OF ELECTROMAGNETIC SHOWER
C     ARGUMENT INPUT
C                   EMGNRJ = SHOWER ENERGY
C                   EMBETA = BETA COEFFICIENT OF SHOWER
C                   EMALFA = ALFA COEFFICIENT OF SHOWER
C                   TINOX0 = CURRENT RADIATION LENGTH
C     ARGUMENT OUTPUT
C                   IER = 0 SHOWER NOT FULLY ABSORBED
C                   IER = 1 SHOWER ABSORBED
C ----------------------------------------------------------------
#ifndef DOC
      PARAMETER (SLEN = 1. , SFRAC = 0.001 , SA0 = 0.0001 )
      PARAMETER (ERADMX = 20.)
C ------------------------------------------------------------------
C
C-       Test shower is decreasing
C
      IER = 0
      A0 = CASHEM(TINOX0,1)
      A1 = CASHEM(TINOX0+SLEN,1)
      IF ( A0.GT.A1.OR.(A0.LT.SA0.AND.TINOX0.GT.ERADMX)) THEN
        XFR = (A0+A1)*SLEN/(2.*EMGNRJ)
        IF ( XFR.LT.SFRAC.OR.A1.LT.SA0) THEN
           IER = 1
        ENDIF
      ENDIF
      RETURN
      END
#endif
