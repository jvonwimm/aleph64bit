      SUBROUTINE VDblHt(View, Wafer, Hit)
C----------------------------------------------------------------------
C! Retroactively change the error when you find a double hit
CKEY VGLOB VDET TRACK
C
C   Author:   Paul Rensing    4-5-95
C
C When you have a double hit in the VDet, the hit uncertainty is
C     larger. This routine finds all VDCO entries which use the given
C     hit, and changes the uncertainty.
C
C Input:
C    integer View   - 1=z, 2=r-phi
C    integer wafer  - the wafer number
C    integer hit    - the hit number
C
C Output:
C    the VDCO bank is modified.
C-----------------------------------------------------------------------
#ifndef DOC
      implicit none
C
      integer view, wafer, hit
C
      real DBLHITERR
      parameter (DBLHITERR = 0.0180)
C
C  Global includes
C
#include "bcs.h"
C
#include "vcpljj.h"
#include "vdcojj.h"
C
      integer NAMIND, NLINK
      external NAMIND, NLINK
C
      integer NamVDCO
      save NamVDCO
      integer indVDCO, indVCPL, indVDXZ
      integer bank, row, row2
      integer vcplWaf(2), vcplHit(2), vdcoErr(2)
C
#include "bmacrod.h"
C
C Data statements
C
      DATA NamVDCO/0/
      DATA vcplWaf/JVCPZB, JVCPXB/
      DATA vcplHit/JVCPNZ, JVCPNX/
      DATA vdcoErr/JVDCSZ, JVDCSR/
C
C  Inline functions
C
#include "bmacro.h"
C
      IF (NAMVDCO .eq. 0) THEN
         NAMVDCO = NAMIND('VDCO')
      END IF
C
      call vrmwf(wafer, view, bank)
      indVDCO = IW(NamVDCO)
      if (indVDCO .le. 0) then
         call RERROR('VDBLHIT', -1, ' VDCO bank is missing')
         return
      end if
C
      do row = 1, LROWS(indVDCO)
         if (wafer .eq. ITABL(indVDCO, row, JVDCWI)
     $        .or. bank .eq. ITABL(indVDCO, row, JVDCWI)) then
            indVCPL = NLINK('VCPL', ITABL(indVDCO, row, JVDCTN))
            do row2 = 1, LROWS(indVCPL)
               if (bank .eq. ITABL(indVCPL,row2,vcplWaf(view)) .and.
     $              hit .eq. ITABL(indVCPL,row2,vcplHit(view))) then
                  RW(KROW(indVDCO,row)+vdcoErr(view)) = DBLHITERR**2
                  goto 10
               end if
            end do
         end if
 10      continue
      end do
C
      return
      end
#endif
