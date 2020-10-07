      FUNCTION GFRAC(XXE,YYE)
C-----------------------------------------------------------------------
C  Input XXE X position cog in the 4X4 towers
C        YYE Y position cog in the 4X4 towers
C     -------
C! Output GFRAC fraction of energy in central 2x2 towers
CKEY GAMPACK
C-----------------------------------------------------------------------
#ifndef DOC
      REAL SAX,SAY,GFRAC,XXE,YYE
      INTEGER J,NBAR,NFRA
      PARAMETER (NBAR=5,NFRA=3)
      DOUBLE PRECISION CUAD(4),XE,YE,PBAR(NBAR),AX,AY,TOT,PFRA(NFRA),
     *                 RAY,FRAC
      DATA PBAR/0.01692,0.2377,3.697,-7.310,6.296/
      DATA PFRA/0.8510,0.6063E-2,-0.2068E-1/
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
