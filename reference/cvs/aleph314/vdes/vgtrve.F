      SUBROUTINE VGTRVE (TE,X,XP,XE,XEP)
C----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Transform a vector with a given transformation
C - Francesco Forti, 17 August 1990
C
C   Transforms a vector according with the transformation given in TF
C   XP = A * X  +  T
C   Where A is the rotation matrix and T is the translation.
C   The error matrices are transformed accordingly (not yet
C   implemented).
C
C - Input:
C   TE(33) / R  Transformation structure in expanded form
C   X(3)   / R  Input vector
C   XE(*)  / R  Input vector correlation matrix
C
C - Output:
C   XP(3)  / R  Transformed vector
C   XEP(*) / R  Transformed vector correlation matrix (not implemented)
C
C   Libraries required: CERNLIBS
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgpaal.h"
      REAL TE(*), X(*), XP(*), XE(*), XEP(*)
      INTEGER I
#include "vgvmcr.h"
#include "vgmacr.h"
C ----------------------------------------------------------------------
C
C     Initialize output to translation vector:
C
      DO I=0,2
        XP(I+1) = TE(JVTETR+I)
      ENDDO
C
C     Do the transformation. See CERNLIB F003
C     ! Matrix element 1,1
C     ! Matrix element 1,2
C     ! Matrix element 2,1
C     ! Vector element
C     ! out vector
C
      CALL RMMPA(3,3,    TE( KVINDX(1,1) ),
     &                   TE( KVINDX(1,2) ),
     &                   TE( KVINDX(2,1) ),
     &                   X(1), X(2),
     &                   XP(1),XP(2) )
C
      RETURN
      END
#endif
