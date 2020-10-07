      SUBROUTINE VGSolv
C ---------------------------------------------------------------------
C! Find the best solution for all VDET pat. rec. components.
CKEY VGLOB VDET TRACK
C
C      Paul Rensing   - 1-11-94
C
C Loop over all VCTL banks and call VGBRUT or VGBRAN to solve each one. 
C
C  INPUT:
C      none
C OUTPUT:
C      Solution banks VGXC and VGHC are created.
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
#include "bcs.h"
C
#include "vglbcm.h"
#include "vglbst.h"
C
C  Function calls
C
      INTEGER NLINK, NAMIND
      EXTERNAL NLINK, NAMIND
C
C  Local variables
C
      INTEGER NamVCTL
      integer indVCTL, indVM5C
      real t1, t2
      real size
      logical solved
      real xx(3)
      integer i
C
      CHARACTER*8 nnames(3)
C
      SAVE NamVCTL
C
#include "bmacrod.h"
C
      DATA NamVCTL/0/
      data nnames/'NTRKS', 'CSIZE', 'SOLVTIME'/
C
#include "bmacro.h"
C
      IF (NamVCTL .EQ. 0) then
         NamVCTL = NAMIND('VCTL')
      end if
C
C Start chaining through the VCTL banks
C
      indVCTL = IW(NamVCTL)
      DO WHILE (indVCTL .gt. 0)
         CALL TIMEX(T1)
C
         solved = .false.
         call vgbrut(iw(indVCTL-2), Solved)
C
         If (.not. Solved) then
            call vgbran(iw(indVCTL-2))
         end if
C
         CALL TIMEX(T2)
         TimSol = TimSol + (T2 - T1)
C
         indVCTL = IW(indVCTL - 1)
      end do
C
      RETURN
      END
#endif
