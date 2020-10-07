      SUBROUTINE VPkSln( comp, bestsoln )
C-------------------------------------------------------------
C! Pick the VDet Pat. Rec. soln from the list of possibilities
CKEY VGLOB VDET TRACK
C
C  Author      : Paul Rensing  17-5-95
C
C This routines takes the list of ambiguous solutions provide by VGSOLV
C     and tries to pick the best using various heuristics.
C
C  INPUTS:
C     integer comp  - the number of the component
C
C  OUTPUTS:
C     integer soln  - the number of the solution to use
C
C-------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C IO variables
C
      integer comp, bestsoln
C
#include "bcs.h"
C
#include "vghcjj.h"
#include "vgxcjj.h"
C
      integer NLINK
      external NLINK
C
      integer indVGHC, indVGXC
      integer row, col
      integer soln
      integer n, maxn
      integer ntrk
      integer trk1, trk2
      real z0_1, z0_2
C
#include "bmacrod.h"
#include "bmacro.h"
C
      bestsoln = 1
C
      indVGXC = NLINK('VGXC', comp)
      if (indVGXC .le. 0) then
         CALL RERROR('VPKSLN', -1, ' Missing VGXC bank')
         return
      end if
C
      if (LROWS(indVGXC) .le. 1) return
      ntrk = ITABL(indVGXC, 1, JVGXNT)
C
      indVGHC = NLINK('VGHC', comp)
      if (indVGHC .le. 0) then
         CALL RERROR('VPKSLN', -2, ' Missing VGHC bank')
         return
      end if
C
C Select the maximum number of 3D hits
C
      maxn = -1
      do soln = 1, LROWS(indVGXC)
         n = 0
         do row = ntrk*(soln-1)+1, ntrk*soln
            do col = JVGHH1, JVGHH4+1, 2
               if (ITABL(indVGHC, row, col) .gt. 0 .and.
     $              ITABL(indVGHC, row, col+1) .gt. 0) then
                  n = n + 1
               end if
            end do
         end do
         if (n .gt. maxn) then
            maxn = n
            bestsoln = soln
         end if
      end do
C
      return
      end
#endif
