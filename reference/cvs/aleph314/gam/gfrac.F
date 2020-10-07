      FUNCTION GFRAC(XXE,YYE)
C-----------------------------------------------------------------------
C  Modified: D.Smith     991025 Double precision fixes for Linux
C            M.Cattaneo  991025 Suppress unused declarations
C
C  Input XXE X position cog in the 4X4 towers
C        YYE Y position cog in the 4X4 towers
C     -------
C! Output GFRAC fraction of energy in central 2x2 towers
CKEY GAMPACK
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      REAL GFRAC,XXE,YYE
      INTEGER J,NFRA
C      PARAMETER (NBAR=5,NFRA=3)
      PARAMETER (NFRA=3)
      DOUBLE PRECISION XE,YE,PFRA(NFRA),RAY,FRAC
C      DATA PBAR/0.01692D0,0.2377D0,3.697D0,-7.310D0,6.296D0/
      DATA PFRA/0.8510D0,0.6063D-2,-0.2068D-1/
C---------------------------------------------------------
Conclude with the fraction
      XE = DBLE(XXE)
      YE = DBLE(YYE)
      RAY = SQRT(XE**2+YE**2)
      FRAC = PFRA(1)
      DO J=2,NFRA
        FRAC = FRAC + PFRA(J)*RAY**(J-1)
      ENDDO
      GFRAC = SNGL(FRAC)
C
      END
#endif
