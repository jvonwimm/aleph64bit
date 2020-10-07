      subroutine vgcmbv(wHits, uHits, hits, ucols, good)
C-------------------------------------------------------------
C! Merge list of hits from both views and check for feasibility
CKEY VGLOB VDET TRACK
C
C  Author      : P. Rensing  27-8-95
C
C The routine is passed the lists of hits (packed bank number + row number)
C    for the two views. It produces a single list of hits (4 (w,u) pairs)
C    and checks if the set is feasible, ie. that it was physically possible.
C
C  INPUTS:
C    INTEGER wHits(4)    = List of w (ie. z) hits
C    INTEGER uHits(4)    = List of u (ie. r-phi) hits
C 
C  OUTPUTS:
C    INTEGER hits(8)     = merged list of hits ( (w,u) pairs)
C    INTEGER ucols(4)     = column number for each U hit 
C    LOGICAL good        = .TRUE. is this set of hits could be from 1 track
C
C-------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C subroutine arguments
C
      integer wHits(4), uHits(4)
      integer hits(8), ucols(4)
      logical good
C
      integer wafxy(2), wafzt(2)
      integer j, ihit, pckhit
      integer jlay
      integer k
      logical used(2), swap
C
C  Declare statement function variables
C
      INTEGER WW, HH, VHPCK
C
C Statement function to create a single integer ID for a hit.
C
      VHPCK(WW, HH) = WW*1000 + HH
C
      do j = 1, 4
         ucols(j) = j
      end do
C
C loop over layers
C
      do jlay = 0, 1
C
C look through the z hits for this track. Map the wafer number as if it
C     where an r-phi hit to make comparison easier.
C
         DO J = 1, 2
            pckhit = whits(jlay*2 + j-1)
            hits( jlay*4 + (j-1)*2 + 1 ) = pckhit
            if (pckhit .gt. 0) then
               call vhuprm(pckhit, 2, ihit, wafzt(j))
            else
               wafzt(j) = -1
            end if
         end do
C
C look through the r-phi hits
C
         DO J = 1, 2
            pckhit = uHits(jlay*2 + j-1)
            hits( jlay*4 + j*2 ) = pckhit
            if (pckhit .gt. 0) then
               call vhuprm(pckhit, 2, ihit, wafxy(j))
            else
               wafxy(j) = -1
            end if
         end do
C
C Make sure that both wafers have a match. The Z index of all wafers in a
C     layer must match and then r-phi index of Z wafers must match at
C     most one of the r-phi wafers. 
C
C
         if ((wafxy(1) .ge. 0 .or. wafxy(2) .ge. 0)
     $        .and. (wafzt(1) .ge. 0 .or. wafzt(2) .ge. 0)) then
            
            used(1) = .false.
            used(2) = .false.
            
            do j = 1,2
               if (wafzt(j) .ge. 0) then
C
C first, check for two real wafers that match
C
                  do k = 1,2
                     if ( (.not.used(k)) .and.
     $                    wafzt(j) .eq. wafxy(k) ) then
                        used(k) = .true.
                        swap = j .ne. k
                        goto 18
                     end if
                  end do
C
C if no match, find a missing hit to pair it.
C
                  do k = 1,2
                     if ( (.not.used(k)) .and.
     $                    wafxy(k) .lt. 0 ) then
                        used(k) = .true.
                        swap = j .ne. k
                        goto 18
                     end if
                  end do
C
C no match, this is an unphysical combination. Give up.
C               
                  good = .false.
                  goto 999
C
               end if
 18            continue
            end do
C
C swap the hits if needed
C
            if (swap) then
               pckhit = hits( jlay*4 + 2 ) 
               hits( jlay*4 + 2 ) = hits( jlay*4 + 4 )
               hits( jlay*4 + 4 ) = pckhit
               pckhit = ucols(jlay*2 + 1)
               ucols(jlay*2 + 1) = ucols(jlay*2 + 2)
               ucols(jlay*2 + 2) = pckhit
            end if
         end if
C
C end of loop over layers
C
      end do
C
      good = .true.
C
C Make sure that missing hits are flagged with a -1.
C Also, make the u wafer number identical to the w wafer number.
C
      do j = 1, 8, 2
         if (hits(j) .ne. 0 .and. hits(j+1) .eq. 0) then
            hits(j+1) = -1
         else if (hits(j+1) .ne. 0 .and. hits(j) .eq. 0) then
            hits(j) = -1
         else if (hits(j+1) .gt. 0 .and. hits(j) .gt. 0) then
            call vhupck(hits(j), wafzt(1), ihit)
            call vhupck(hits(j+1), wafxy(1), ihit)
            hits(j+1) = VHPCK(wafzt(1), ihit)
         end if
      end do
 999  continue

      return
      end
#endif
