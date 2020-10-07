      SUBROUTINE VHMAP(SelFlag)
C ---------------------------------------------------------------------
C! Create a map of hit to track for the VDet global pat. rec.
CKEY VGLOB VDET TRACK
C
C      Paul Rensing   - 1-11-94
C
C For the set of track indicated by flag SelFlag (column 4 in FXTR), 
C      create the set of VHMx banks which map hits to tracks.
C
C INPUT :
C      INTEGER SelFlag  = track selection flag
C OUTPUT:
C      Bank VHMx are created, x = 1-4
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
      INTEGER SelFlag
C
C  Global includes
C
#include "bcs.h"
#include "vglbcm.h"
C
#include "fxtrjj.h"
#include "vm5cjj.h"
#include "vhmpjj.h"
C
C  Function calls
C
      INTEGER NLINK,NBANK, NAMIND
      EXTERNAL NLINK,NBANK, NAMIND
C
C  Local variables
C
      INTEGER indVM5C, indFXTR
      INTEGER row, k, HIT, itrk
      integer col, view
      INTEGER INDH
      LOGICAL ADDED
      integer NamVM5C, NamFXTR
C
      save NamVM5C, NamFXTR
C
#include "bmacrod.h"
C
C Data Statements
C
      DATA NamVM5C/0/
C
C  Inline functions
C
#include "bmacro.h"
C
      if (namVM5C .eq. 0) then
         namVM5C = NAMIND('VM5C')
         namFXTR = NAMIND('FXTR')
      end if
      indFXTR = IW(NamFXTR)
      if (indFXTR .le. 0) then
         call RERROR('VHMAP', -1, ' Missing FXTR bank')
         goto 999
      end if
C
C Chaining through the VM5C bank
C
      indVM5C = IW(namVM5C)
      do while (indVM5C .gt. 0)
         itrk = IW(indVM5C-2)
         if (ITABL(indFXTR, itrk, JFXTVD) .eq. SelFlag) then
C
C For each track, loop through each assignment hypothesis and 
C  each hit in that hypothesis. For each hit, add the track number
C  to the list (if not already done).
C
            DO row = 1, LROWS(indVM5C)
               do col = 1, NLAYER*NGRK*NVIEW, NVIEW
                  do view = 1, NVIEW
                     k = col-1 + view-1
                     hit = ITABL(indVM5C, row, JVM5HW + K)
                     IF (HIT .GT. 0) THEN
                        CALL VHITID(hit, view, hit)
                        INDH = NLINK('VHMP', HIT)
                        IF (INDH .EQ. 0) THEN
                           INDH = NBANK('VHMP', HIT, LMHLEN+10*LVHMPA)
                           if (indh .eq. 0) then
                              call rerror('VHMAP', -2,' Out of memory')
                              goto 999
                           end if
                           IW(INDH+LMHCOL) = 1
                           IW(INDH+LMHROW) = 0
                        END IF
                        CALL VADDIA(INDH, itrk, ADDED )
                     END IF
                  end do
               end do
            END DO
         end if
C         
C end of loop over rows in VM5C
C
         indVM5C = IW(indVM5C - 1)
      END do
C
 999  continue
      RETURN
      END
#endif
