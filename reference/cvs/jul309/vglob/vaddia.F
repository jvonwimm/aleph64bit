      SUBROUTINE VADDIA(IND, VAL, ADDED)
C ---------------------------------------------------------------------
C! Add value VAL to bank IND.
C
C      Paul Rensing   - 1-11-94
C
C VADDIA expects IND to point to a BOS bank. The bank is first checked
C     if VAL is already in the bank (see VFNDIL). If not, a new row is
C     created with VAL in the first column. The bank may be extended
C     but NO garbage collection is allowed. ADDED is returned .TRUE. if
C     the element was added, .FALSE. otherwise.
C
C See Also: VFNDIL
C
C  INPUT:
C     INTEGER IND   - bank index
C     INTEGER VAL   - integer value
C OUTPUT:
C     INTEGER IND   - may be changed if item is added
C     LOGICAL ADDED - .TRUE. if VAL was added to list
C     Bank (IND) may be modified
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C Subroutine parameters
C 
      INTEGER IND, VAL
      LOGICAL ADDED
C
C  Global includes
C
#include "bcs.h"
C
C  Function calls
C
      INTEGER NBANK
      CHARACTER*4 CHAINT
      EXTERNAL NBANK, CHAINT
C
C  Local variables
C
      INTEGER LOC, IND2
C
#include "bmacrod.h"
#include "bmacro.h"
C
      CALL VFNDIL(IND, VAL, LOC)
      IF (LOC .GT. 0) THEN
         ADDED = .FALSE.
         RETURN
      ELSE
C 
C Not found, extend the list
C
         ADDED = .TRUE.
         IF (LFRROW(IND) .EQ. 0) THEN
            IND2 = NBANK(CHAINT(IW(IND-3)), IW(IND-2),
     $           IW(IND)+10*LCOLS(IND))
            IF (IND2 .EQ. 0) THEN
               CALL RERROR('VADDIA', -1, ' Out of memory')
               RETURN
            END IF
            IND = IND2
         END IF
         IW(KNEXT(IND)+1) = VAL
         IW(IND+LMHROW) = IW(IND+LMHROW) + 1
      END IF
C
      RETURN
      END
#endif
