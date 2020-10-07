      SUBROUTINE JETERR(NJET,JETS,JET_SIG2)
CKEY   QFNDIP  QIPBTAG / INTERNAL
C ------------------------------------------------------------------------------
C! Error on jet angular resolution
C  Give the error on the jet angular resolution (relative
C  to the b meson in Zbb events) as a function of their
C  momentum.  Dave Brown, 15-10-93
C  Called from FINDIP or FINDDMIN
C
C  INPUTS; NJET, JETS   :  # and jet momentum vectors
C
C  OUTPUTS; JET_SIG2    :  Array of squares of jet angular resolution (radians)
C ------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      INTEGER NJET
      REAL JETS(3,*)
      REAL JET_SIG2(*)
C
C  Variables to define jet resolution; put these in common as they
C  change depending on the jet algorithm used
C
      REAL JRES(3),JMAX,JSCALE
      COMMON/JSIG/JRES,JMAX,JSCALE
C
C  Local variables
C
      INTEGER ICOR,IJET
      REAL JP,JERR
C
C  Local copy of common variables for data initialization
C
      REAL L_JRES(3),L_JMAX,L_JSCALE
      LOGICAL FIRST
C
C  Default values
C
      DATA L_JRES/0.2862,-.01205,0.000135/
      DATA L_JMAX/45.0/,L_JSCALE/4.0/
      DATA FIRST/.TRUE./
C
C  Inline for the jet error parameterization
C
      JERR(JP) = JRES(1)+JRES(2)*MIN(JP,JMAX)+
     &      JRES(3)*MIN(JP,JMAX)**2
C ------------------------------------------------------------------------------
C
C  First time through, copy the default parameters
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        DO ICOR=1,3
          JRES(ICOR) = L_JRES(ICOR)
        END DO
        JMAX = L_JMAX
        JSCALE = L_JSCALE
      END IF
C
C  Normalize the jets, get their momenta and thereby their angular
C  resolution
C
      DO IJET=1,NJET
        JP = 0.0
        DO ICOR=1,3
          JP = JP + JETS(ICOR,IJET)**2
        END DO
        JP = SQRT(JP)
        JET_SIG2(IJET) = JSCALE*JERR(JP)**2
      END DO
      RETURN
      END
#endif
