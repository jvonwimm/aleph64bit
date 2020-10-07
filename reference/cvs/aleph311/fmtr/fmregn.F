      SUBROUTINE FMREGN (VEC,IRG)
C-----------------------------------------------------------------
C!  This routine finds the current volume given the position of a
C!  particle.
C-----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "fmgeom.h"
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
#endif
