      SUBROUTINE VGCMTR (TE1,TE2,TECM)
C----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Combine two transformation structures
C - Francesco Forti, 17 August 1990
C
C   Combines two transformation structures to give a single
C   transformation.  TE1 is applied FIRST and TE2 is applied
C   afterwards.  Calling A the rotation matrix and T the translation
C   vector, the first transformation is XP = A*X + T. The second is
C   XPP = B*XP + S where TE1=(A,T) and TE2=(B,S).  The complete
C   transformation will be XPP = B*(A*X+T) + S = B*A*X + (B*T + S),
C   i.e., TECM = (B*A,B*T+S).
C
C - Input:
C   TE1(33)  / R  First transformation structure
C   TE2(33)  / R  Second transformation structure
C
C - Output:
C   TECM(33) / R  Output transformation structure
C
C   Libraries required: CERNLIBS
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgpaal.h"
      REAL TE1(*), TE2(*), TECM(*)
      REAL DUMMY
#include "vgvmcr.h"
#include "vgmacr.h"
C ----------------------------------------------------------------------
C
C     Initialize TECM = TE2:
C
      CALL UCOPY(TE2, TECM, LVTEXP)
C
C     Matrix multiplication (see CERNLIB F004):
C
      CALL RMMLT( 3,3,3,
     &  TE2(KVINDX(1,1)),TE2(KVINDX(1,2)),TE2(KVINDX(2,1)),
     &  TE1(KVINDX(1,1)),TE1(KVINDX(1,2)),TE1(KVINDX(2,1)),
     &  TECM(KVINDX(1,1)),TECM(KVINDX(1,2)),TECM(KVINDX(2,1)),
     &  DUMMY)
C
C     Calculate the new translation vector.
C     The translation vector in TECM is currently .EQ. S
C
      CALL RMMPA( 3,3,
     &  TE2(KVINDX(1,1)),TE2(KVINDX(1,2)),TE2(KVINDX(2,1)),
     &  TE1(JVTETR), TE1(JVTETR+1),
     &  TECM(JVTETR), TECM(JVTETR+1)  )
C
      RETURN
      END
#endif
