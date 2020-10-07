      SUBROUTINE PI0LIM(E,AMEAN,SIGM)
CKEY  QPI0DO / INTERNAL
C-----------------------------------------------------------------------
C! Defines the mass cut used to find pi0 candidates
C  Called from QPI0DO
C input : E gamma-gamma energy
C output: AMEAN mean reconstructed pi0 mass from gampec
C         SIGM  reconstructed pi0 mass width from gampec
C given photon pair energy E compute mass limits for defining
C pi0 candidate taking into account GAMPEC mass shift and resolution
C Author J-P Lees  15 - Oct - 1992
C-----------------------------------------------------------------------
#ifndef DOC
      IF (E.LT.8.) THEN
        AMEAN = .13497
      ELSE
        AMEAN = .13497 +(E-8.)*.002
      ENDIF
C.. Compute sigma
      IF (E.LT.4.) THEN
        SIGM  = .036 -E*.004
      ELSEIF (E.LT.14.) THEN
        SIGM  = .020
      ELSE
        SIGM  = .020+(E-14.)*.0012
      ENDIF
C
 999  RETURN
      END
#endif
