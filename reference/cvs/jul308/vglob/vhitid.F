      SUBROUTINE VHITID(wafHit, view, hitID)
C ---------------------------------------------------------------------
C!Map packed wafer/hit to unique hit ID
CKEY VGLOB VDET
C      Paul Rensing   - 28-8-95
C
C Given a packed wafer number / hit number (see VHPCK), return a unique
C     ID for the hit. This is the cluster number within the event, so it
C     is a reasonable array index.
C
C  INPUT:
C       integer wafHit   - the packed wafer,hit numbers
C       integer view     - the view ( 1 = Z, 2 = r/phi )
C OUTPUT:
C       integer hitID    - hit id
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
      integer wafHit, view, hitID
C
C  Global includes
C
#include "bcs.h"
C
#include "vdmrjj.h"
C
      integer NLINK
      external NLINK
C
      integer wafer, hit, bank, indVDMR
C
#include "bmacrod.h"
#include "bmacro.h"
C
      if (wafhit .gt. 0) then
         call vhuprm(wafHit, view, hit, bank)
         indVDMR = NLINK('VDMR', bank+view-1)
         if (indVDMR .le. 0) then
            call rerror('VHITID', -1, ' VDMR bank is missing')
            hitID = 0
            return
         end if
         hitID = ITABL(indVDMR, hit, JVDMVD)
      else
         hitID = wafhit
      end if
C
      return
      end
#endif
