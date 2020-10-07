      SUBROUTINE VFNDIL( IND, VAL, LOC)
C ---------------------------------------------------------------------
C! Find VAL in the list IND and return its index
CKEY VGLOB
C      Paul Rensing   - 2-11-94
C
C VFNDIL expects IND to point to a BOS bank. It searches the bank for
C     value VAL in the first column and returns the row number. It
C     returns -1 if VAL is not found.
C
C  INPUT:
C    INTEGER VAL     - value to find
C    INTEGER IND     - BOS index of bank
C  
C OUTPUT:
C    INTEGER LOC     - index (row) of VAL in the bank
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C Parameters
C
      INTEGER VAL, LOC
      INTEGER IND
C
C  Global includes
C
#include "bcs.h"
C
      INTEGER I
C
#include "bmacrod.h"
#include "bmacro.h"
C
      LOC = -1
      DO I = 1, LROWS(IND)
         IF (ITABL(IND,I,1) .EQ. VAL) THEN
            LOC = I
            RETURN
         END IF
      END DO
C
      RETURN
      END
#endif
