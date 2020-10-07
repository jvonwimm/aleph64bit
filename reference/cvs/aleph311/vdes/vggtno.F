      SUBROUTINE VGGTNO(ILAY,IWFF,IFAC,TENO)
C ----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Get the nominal transformation for a wafer
C - Francesco Forti, 18 August 1990
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C
C   Returns the transformation to the nominal position of a wafer, in
C   the sense that if TE=(A,T), where A is the rotation matrix and T is
C   the translation, the transformation is defined by:  X = A*U + T.
C   X is the vector in the Aleph main frame, while U is the vector in
C   the wafer local coordinate system, U=(v,u,w).  For a point in the
C   wafer plane, v = 0.
C
C - Input:
C   ILAY     / I  Layer index
C   IWFF     / I  Local wafer-in-face index
C   IFAC     / I  Local face index
C
C - Output:
C   TENO(33) / R  Nominal transformation for the wafer
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgpaal.h"
C
      INTEGER ILAY, IWFF, IFAC, JWAF, IRET, JFAC
      INTEGER VSETUP, VJWFFW, VXYZWA, VJFACI, VPHIFN
      REAL TENO(*)
      REAL PHIN, CPHIN, SPHIN
C
C Unity transformation
C
      REAL TUNIT(LVTEXP)
C
C  Data statements
C
      DATA TUNIT / 0., 0., 0.,
     >             1., 0., 0., 0., 1., 0., 0., 0., 1.,
     >             LVEMLN*0. /
C
#include "vgvmcr.h"
#include "vgmacr.h"
C ----------------------------------------------------------------------
C
C     Preset the vector:
C
      CALL UCOPY(TUNIT,TENO,LVTEXP)
C
C     Check that the geometry package is initialized:
C
      IF (VSETUP() .GT. 0) THEN
C
C     Fill the translation vector, i.e., the center of the wafer:
C
        IRET = VJWFFW(ILAY,IFAC,IWFF,JWAF)
        IRET = VXYZWA(JWAF,TENO(JVTETR+0))
C
C     Fill the matrix with the coordinates of three basis vectors
C     in the order v, u, w.  The rotation matrix is given by
C            (         )
C       RO = ( v  u  w ),
C            (         )
C     v,u,w being column vectors that represent in xyz coordinates the
C     three basis vectors of the wafer local reference frame.
C
C     Direction of wafer normal:
C
        IRET = VJFACI(ILAY,IFAC,JFAC)
        IRET = VPHIFN(JFAC,PHIN)
        CPHIN = COS(PHIN)
        SPHIN = SIN(PHIN)
C
C     v: Normal to wafer
C
        TENO(KVINDX(1,1)) = CPHIN
        TENO(KVINDX(2,1)) = SPHIN
        TENO(KVINDX(3,1)) = 0.
C
C     w: equals z
C
        TENO(KVINDX(1,3)) = 0.
        TENO(KVINDX(2,3)) = 0.
        TENO(KVINDX(3,3)) = 1.
C
C     u: phi direction
C
        TENO(KVINDX(1,2)) = -SPHIN
        TENO(KVINDX(2,2)) = CPHIN
        TENO(KVINDX(3,2)) = 0.
C
      ENDIF
C
      RETURN
      END
#endif
