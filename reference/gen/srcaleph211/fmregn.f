      SUBROUTINE FMREGN (VEC,IRG)
C-----------------------------------------------------------------
C!  This routine finds the current volume given the position of a
C!  particle.
C-----------------------------------------------------------------
      SAVE
      PARAMETER (NRGION = 11)
      COMMON/FMGEOM/FMZMIN(NRGION),FMZMAX(NRGION),FMRMIN(NRGION),
     &              FMRMAX(NRGION)
      COMMON/FMSMUL/FMRADL(NRGION),SINMUL(101),COSMUL(101),SQRMUL(101)
      COMMON/FMELSS/FRATIO(NRGION),CONBB1(NRGION),CONBB2(NRGION),XMUMAS,
     &              XMUMS2
C
C
      DIMENSION VEC(3)
C
      IRG = -1
C
      X = VEC (1)
      Y = VEC (2)
      Z = VEC (3)
      RAD = SQRT (X**2 + Y**2)
C
C  Now see which region we are in
C
      DO 100 J = 1, NRGION
         RMIN = FMRMIN (J)
         RMAX = FMRMAX (J)
         ZMAX = FMZMAX (J)
         ZMIN = FMZMIN (J)
C
         IF ((ABS(Z).LE.ZMAX).AND.(ABS(Z).GE.ZMIN).AND.
     1     (RAD.GE.RMIN).AND.(RAD.LE.RMAX)) THEN
            IRG = J
            RETURN
         ENDIF
C
  100 CONTINUE
      RETURN
      END
