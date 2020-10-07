      SUBROUTINE VGFEVT
C ---------------------------------------------------------------------
C! Clean up after all VDet pattern recognition is done for event.
CKEY VGLOB VDET TRACK
C
C      Paul Rensing   - 21-6-95
C
C Final VDet pattern recognition cleanup. Change all NAss in VDXY/VDZT
C     to abs(NAss).
C
C OUTPUT:
C      VDXY, VDZT are modified
C      VDCO, FVCL are shrunk to size
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  Global includes
C
#include "bcs.h"
#include "vglbcm.h"
#include "vglbst.h"
C
#include "vdxyjj.h"
#include "vdztjj.h"
C
C  Function calls
C
      INTEGER NAMIND, NLINK, NBANK, VDYEAR
      EXTERNAL NAMIND, NLINK, NBANK, VDYEAR
C
C  Local variables
C
      INTEGER INDEX
      INTEGER row, jNa, view
      INTEGER namVDXZ(NVIEW), NAssCol(NVIEW)
      SAVE namVDXZ, NAssCol
C
#include "bmacrod.h"
C
C Data statements
C
      DATA namVDXZ/ 0, 0 /
      DATA NAssCol/ JVDZNA, JVDXNA /
C
#include "bmacro.h"
C
      IF (NAMVDXZ(1) .LE. 0) THEN
         NAMVDXZ(1) = NAMIND('VDZT')
         NAMVDXZ(2) = NAMIND('VDXY')
      END IF
C
C Shrink the VDCO and FVCL banks
C
      INDEX = NLINK('VDCO',0)
      if (LROWS(index) .gt. 0) then
         index = NBANK('VDCO',0, LMHLEN + LCOLS(index)*LROWS(index))
      else
         call bdrop(IW, 'VDCO')
      end if
      INDEX = NLINK('FVCL',0)
      if (LROWS(index) .gt. 0) then
         index = NBANK('FVCL',0, LMHLEN + LCOLS(index)*LROWS(index))
      else
         call bdrop(IW, 'FVCL')
      end if
C
C Reset NAss in VDXY/VDZT to abs(NAss)
C
      Do view = 1, NVIEW
         INDEX = IW(NAMVDXZ(view))
         DO WHILE (INDEX .GT. 0)
            DO row = 1, LROWS(INDEX)
               jNa = KROW(INDEX,row)+NAssCol(view)
               IW(jNa) = abs(IW(jNa))
C
C assignment statistics
C
               NHitVD(view) = NHitVD(view) + 1
               If (IW(jNa) .gt. 0) Then
                  NHitAs(view) = NHitAs(view) + 1
                  If (IW(jNa) .gt. 1) Then
                     NHitDb(view) = NHitDb(view) + 1
                  End If
               End If
            END DO
            INDEX = IW(INDEX - 1)
         END DO
      End Do
C
C Remove VDMR is not VDET 95
C
      if (VDYEAR() .ne. 95) then
         call BDROP(IW, 'VDMR')
      end if
      call BDROP(IW, 'VGGW')
C
C for debugging, do not drop VM5C banks (but they are on E list for cleanup)
C
      Call BDROP(IW, 'VM5C')
C
C Clear ITC banks
C
CC      call ISETUP(1)
C
      RETURN
      END
#endif
