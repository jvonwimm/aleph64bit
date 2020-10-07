      SUBROUTINE REJECT(NUMRU,NUMEV)
C--------------------------------------------------------------------
C!Decide what to do with misplaced events
C
C    Author S.Orteu       16-DEC-1987
C
C
C--------------------------------------------------------------------
#ifndef DOC
#include "rcurnt.h"
#include "rlunit.h"
#include "rflags.h"
C Default action: Print warning and set IGET=0
      IF (FDEBRF) THEN
        WRITE(LOUTRL,'(A)') '0REJECT- Warning'
        WRITE(LOUTRL,'(A,I8,A,I8)') ' Event',NUMEV,' from run ',NUMRU
        WRITE(LOUTRL,'(A,I8,A)')
     *    ' reconstructed with current constants (Run',IRUNRC,')'
      ENDIF
      END
#endif
