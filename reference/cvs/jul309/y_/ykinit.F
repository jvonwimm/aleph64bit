      subroutine YKInit
C----------------------------------------------------------------
C! Initialize the cuts used in the Kink analysis
C
C     Uses banks YKCI and YKSP
C
C       Author: Paul Rensing    08/04/97
C
C----------------------------------------------------------------
#ifndef DOC

      implicit none

#include "bcs.h"
#include "yknkcm.h"
#include "ykcijj.h"
#include "ykspjj.h"

      integer NAMIND, ALGTDB
      external NAMIND, ALGTDB

      integer indYKCI, indYKSP, namYKCI, namYKSP
      integer i, irun, ievt, dum, JUNIDB, iret

#include "bmacrod.h"

#include "bmacro.h"
      DATA namYKCI / 0 /

      if (namYKCI .eq. 0) then
        namYKCI = NAMIND('YKCI')
        namYKSP = NAMIND('YKSP')
      endif

      call ABRUEV(irun, ievt)

      iret = ALGTDB(JUNIDB(DUM),'YKCIYKSP',irun)
      indYKCI = IW(namYKCI)
      if (indYKCI .le. 0) then
         call RERROR('YKINIT', 2, ' Missing YKCI bank')
         return
      end if
      indYKSP = IW(namYKSP)
      if (indYKSP .le. 0) then
         call RERROR('YKINIT', 2, ' Missing YKSP bank')
         return
      end if
      if (LCOLS(indYKSP) .lt. LYKSPA) then
         call RERROR('YKINIT', 2, ' Missing YKSP bank')
         return
      end if
      ykNSpec = LROWS(indYKSP)
      if (ykNSpec .gt. YKMXSPEC) then
         call RERROR('YKINIT', 3, ' Too many rows in YKSP bank')
         ykNSpec = YKMXSPEC
      end if
         
      fidInnerRad = RTABL(IndYKCI, 1, JYKCFI)
      fidOuterRad = RTABL(IndYKCI, 1, JYKCFO)
      fidMaxZ = RTABL(IndYKCI, 1, JYKCFZ)
      
      minDistXYCut = RTABL(IndYKCI, 1, JYKCMX)
      minDistZCut = RTABL(IndYKCI, 1, JYKCMZ)
      maxBadHits = ITABL(IndYKCI, 1, JYKCNB)
      firstChgCut = RTABL(IndYKCI, 1, JYKCCC)
      
C max chi^2 per dof on track fits
      chi2TrkCut = RTABL(IndYKCI, 1, JYKCTC)
      chi2VtxCut = RTABL(IndYKCI, 1, JYKCVC)
      
      vdetChiCut = RTABL(IndYKCI, 1, JYKCVD)
      vdetChiDiff = RTABL(IndYKCI, 1, JYKCV2)
C
C Kink Species and mass window cuts
C
      do i = 1, ykNSpec
         YKParM(i) = RTABL(indYKSP, i, JYKSPM)
         YKDauM(i) = RTABL(indYKSP, i, JYKSDM)
         YKNeutM(i) = RTABL(indYKSP, i, JYKSNM)
         YKTagC(i) = RTABL(indYKSP, i, JYKSTC)
         YKMCut(i) = RTABL(indYKSP, i, JYKSMC)
      end do

C Make sure that the VGLOB package is initialized
C      call vginit

      return
      end
      
#endif
