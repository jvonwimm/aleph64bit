      SUBROUTINE VGINTR (TE,TEINV,IFAIL)
C----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Invert a transformation structure
C - Francesco Forti, 17 August 1990
C
C   Inverts a transformation structure. The direct transformation is
C   defined as XP = A*X + T.  The inverse will be (B = A**-1)
C   X = B*(XP-T) = B*XP - B*T.
C   If the matrix A is singular, the IFAIL flag will be negative and
C   the TEINV structure will be undefined.
C
C - Input:
C   TE(33)    / R  Transformation structure
C
C - Output:
C   TEINV(33) / R  Output transformation structure
C   IFAIL     / I  =0 if OK;
C                  =-1 if singular matrix
C
C   Libraries required: CERNLIBS
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgpaal.h"
      REAL TE(*), TEINV(*)
      REAL WORK(3)
      INTEGER I, IFAIL
C
C ----------------------------------------------------------------------
C
C     Initialize output to form to input form:
C
      CALL UCOPY(TE,TEINV,LVTEXP)
C
C     Proceed with transformation.
C     Change sign of translation:
C
      DO I=0,2
        TEINV(JVTETR+I) = -TEINV(JVTETR+I)
      ENDDO
C
C     Now perform inversion (see CERNLIB F010):
C
      CALL REQINV(3, TEINV(JVTERO), 3, WORK,
     &            IFAIL, 1, TEINV(JVTETR) )
C
      RETURN
      END
#endif
