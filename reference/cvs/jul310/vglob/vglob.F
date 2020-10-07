      SUBROUTINE VGLOB(FIELD, SELFLAG)
C-------------------------------------------------------------------------
C! VDET global pattern recognition
CKEY VGLOB VDET TRACK
C
C  Author      : D. Brown 12-11-94
C
C
C  INPUTS:
C     REAL FIELD       = Magnetic field
C     INTEGER SELFLAG  = track selection flag (col 4 in FXTR)
C     bank FRFT, 0
C              
C  OUTPUTS:
C     VGHC and VGXC banks for this set of tracks. First new bank is 
C         numbered 1 higher than existing banks.
C
C-------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C IO variables
C
      REAL FIELD
      INTEGER SelFlag
C
#include "bcs.h"
C
#include "vglbcm.h"
C
      integer NAMIND, NLINK
      external NAMIND, NLINK
C
C  Local variables
C
      integer i
      integer CompOffset, soln
      real oldfpb
      integer indVGHC, NamVGHC
C
      save NamVGHC
C
      DATA NamVGHC/0/
C
      if (NamVGHC .eq. 0) then
         NamVGHC = NAMIND('VGHC')
      end if
C
C turn off any filtering in the Kalman, but save the old filter probability
C
      oldfpb = 0.0
      call UFTFIL(oldfpb)
C
C Find the last component number used
C
C bank# >10000 are fake solutions
C
      indVGHC = IW(NamVGHC)
      CompOffset = 0
      do while (indVGHC .gt. 0 .and. IW(indVGHC - 2) .lt. 10000)
         CompOffset = IW(indVGHC - 2)
C
         indVGHC = IW(indVGHC - 1)
      end do
C
C Find all the matching hit patterns for the good tracks in both view
C  3rd parameter says to use Kalman; this is added to allow user to 
C     call VM1TRK if it is useful, but without refitting the track.
C
      CALL VM1TRK(FIELD, SelFlag, .TRUE.)
C
C  For these tracks, compute the components
C
      CALL VGCOMP(CompOffset, SelFlag)
C
C  Solve the components
C
      CALL VGSOLV
C
C  Create the VDCO, VCPL banks. This does not refit the tracks.
C
C bank# >10000 are fake solutions
C
      indVGHC = NLINK('VGHC', CompOffSet+1)
      do while (indVGHC .gt. 0 .and. IW(indVGHC - 2) .lt. 10000)
         soln = 1
         If (PckSln) then
            CALL VPkSln(IW(indVGHC - 2), soln)
         end if
         CALL VGSTOR(IW(indVGHC - 2), soln)
C
         indVGHC = IW(indVGHC - 1)
      end do
C
C Delete all component related banks
C
      CALL BDROP(IW,'VCTLVCH1VCH2VCH3VCH4')
C
C reset Kalman filtering probability
C
      call UFTFIL(oldfpb)
C
      RETURN
      END
#endif
