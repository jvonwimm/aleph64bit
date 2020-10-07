      SUBROUTINE VGLINK
C----------------------------------------------------------------------
C! Steering routine for VDet Global Pattern Recognition
CKEY VGLOB VDET TRACK
C
C   Author: Paul Rensing   4-5-95
C   Modified: Dave Casper  17-6-97  Create FXTR if it doesn't exist;
C                                   primarily to facilitate ALPHA interface
C-------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
#include "bcs.h"
#include "vglbcm.h"
#include "vglbst.h"
#include "rconds.h"
C
C Variables from rconds.h, thanks to the stupid IMPLICIT NONE above
C
      real FIELRC,ENERRC
C
C Parameters
C
#include "fxtrjj.h"
C the FRFT bank number to create
      integer FRFTNUM
      parameter (FRFTNUM = 2)
C flag for position of final helix parameters (see UFTKAL.f)
      integer ISWIM
      parameter (ISWIM = 1)
C
      INTEGER NAMIND
      EXTERNAL NAMIND
C
C Local Variables
C
      integer ier, row
      integer trkflag, maxflag
      integer namFXTR, indFXTR
      real T1, T2
      integer irun, lrun, ievt
C
      save namFXTR
C
#include "bmacrod.h"
C
      data namFXTR/0/
      data lrun/0/
C
C  Inline functions
C
#include "bmacro.h"
C
C  First, get the parameters needed for the pat. rec
C
      call abruev(irun, ievt)
      if (irun.ne.lrun) then
        call vginit
        lrun = irun
      endif
C
C  Link to the track selection bank, or unpack it if necessary
C
      if (namFXTR .le. 0) namFXTR = NAMIND('FXTR')
      indFXTR = IW(namFXTR)
      if (indFXTR.le.0) CALL PJPFXT(indFXTR)
      if (indFXTR .le. 0) then
        CALL RERROR('VGLINK',1,'Unable to create track selection bank')
        return
      endif
C
      call TIMEX(T1)
C
C Create the new banks that will be filled; eg. FRFT, VDCO, FVCL
C
      CALL VGNEVT(FRFTNUM)
C
C Select the outer tracks to be used in the pattern recognition.
C
      CALL VSLTRK
C
C Create the track extrapolation banks (VTXT, VTER, VDMS)
C
      CALL VTRKEX(FIELRC,IER)
      IF(IER.EQ.1)
     $     CALL RERROR('VTRKEX',IER,' Iterations step too large')
      IF(IER.EQ.2)
     $     CALL RERROR('VTRKEX',IER,' Iterations not converging')
      IF(IER.EQ.3)
     $     CALL RERROR('VTRKEX',IER,' Iteration result not consistant')
      IF(IER.EQ.4)
     $     CALL RERROR('VTRKEX',IER,' Cylinder extrapolation failed')
      IF(IER.EQ.5)
     $     CALL RERROR('VTRKEX',IER,' No FRFT or FRTL banks')
      IF(IER.EQ.6)
     $     CALL RERROR('VTRKEX',IER,' Not enough BOS space')
C
C Call the new VDET pattern recognition.
C Loop over all allowed settings of the track selection flag.
C The final result is the solution banks (VGHC, VGXC), and the VDet hit
C     banks are filled with the best solution for each track.
C
      trkflag = 0
      maxflag = 0
      do while (trkflag .le. maxflag)
         CALL VGLOB(FIELRC, trkflag)
         trkflag = trkflag + 1
C
         if (trkflag .gt. maxflag) then
            do row = 1, LROWS(indFXTR)
               if (ITABL(indFXTR, row, JFXTVD) .gt. maxflag)
     $              maxflag = ITABL(indFXTR, row, JFXTVD)
            end do
         end if
      end do
C
C Refit all the tracks with VDet hits
C
      CALL VFtAll(FIELRC, FRFTNUM, ISWIM)
C
C Clean up after
C
      call VGFEVT
C
C  Compress Monte Carlo truth cluster to FKIN tracks relation
C
      IF (IW(NAMIND('VTRS')) .GT. 0) THEN
         CALL VDMCTR(IER)
         IF (IER .NE. 0) Then
            CALL RERROR('VDMCTR', IER+10,
     $           ' Bad return code from VDMCTR')
         END IF
      END IF
C
      call TIMEX(T2)
      TimTot = TimTot + (T2 - T1)
      NEvtVD = NEvtVD + 1
C
      RETURN
      END
#endif
