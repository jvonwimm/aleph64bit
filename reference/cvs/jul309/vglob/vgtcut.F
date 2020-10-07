      Subroutine VGTCUT(NTrk, Trks, FootCut, NRemove, RmList)
C-------------------------------------------------------------
C! Remove tracks from components based on size of extrapolation footprint
CKEY VDET TRACK
C
C  Author      : P. Rensing  16-5-95
C
C This routine takes an array of track numbers, assumed to be a VGLOB
C      component, and creates a list of those whose extrapolation 
C      footprint (in r-phi direction) is much larger than the smallest
C      in the component.
C
C  INPUTS:
C    INTEGER NTRK      = number of tracks
C    INTEGER TRKS(*)   = FRFT track number
C    REAL FootCut      = ratio between footprints at which to cut
C 
C  OUTPUTS:
C    INTEGER NREMOVE   = number of tracks to remove
C    INTEGER RMLIST(*) = list of tracks to remove
C
C-------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
      integer NTrk, Trks(*)
      real FootCut
      integer NRemove, RmList(*)
C
C  Global includes
C
#include "bcs.h"
C
#include "vtxtjj.h"
C
C  Function calls
C
      INTEGER NDROP,NLINK,NBANK, NAMIND
      EXTERNAL NDROP,NLINK,NBANK, NAMIND
C
      integer NamVTXT
      integer indVTXT, indl
      real sizeMin
      integer itrk, minTrk
      integer iret
C
      save namVTXT
C
#include "bmacrod.h"
C
      data NamVTXT/0/
C
C  Inline functions
C
#include "bmacro.h"
C
      if (NamVTXT .eq. 0) then
         NamVTXT = NAMIND('VTXT')
      end if
C
      nremove = 0
      sizeMin = 1e6
C
      indl = NBANK('VGXX', 1800, ntrk)
      if (indl .le. 0) then
         call RERROR('VGTCUT', -1,' Out of memory')
         goto 999
      endif
C
      do itrk = 1, ntrk
         indVTXT = NLINK('VTXT', Trks(itrk))
         if (indVTXT .le. 0) then
            call RERROR('VGTCUT', -2, ' Missing VTXT bank')
            goto 999
         end if
C
         RW(indl+itrk) = RTABL(indVTXT, 1, JVTXSU)
         if (RW(indl+itrk) .lt. sizeMin) then
            sizeMin = RW(indl+itrk)
            minTrk = itrk
         end if
      end do
C
      do itrk = 1, ntrk
C 
C allow user to set FootCut=0 to mean keep only 1 track (the best).
C this reproduces the old sequential algorithm
C
         if (itrk .ne. minTrk .and.
     $        RW(indl+itrk) .gt. FootCut * sizeMin) then
            nremove = nremove + 1
            RmList(nremove) = Trks(itrk)
         end if
      end do
C  
 999  continue
      iret = ndrop('VGXX', 1800)
      return
      end
#endif
