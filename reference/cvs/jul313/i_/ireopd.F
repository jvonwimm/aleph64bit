      SUBROUTINE IREOPD(IERR)
C-----------------------------------------------------------------------
C! Reorder the packed digits.(PIDI) according to tracks (FICL).
C!
C!    Author     :- I. Tomalin    88/08/25
C!    Modified   :- J. Sedgbeer   89/04/11  Cosmetic changes.
C!    Modified   :- I. Tomalin    89/06/01
C!
C!    Input:
C!      commons:       /BCS/    for banks  PIDI and FICL
C!                     /RCURNT/ run and event number
C!
C!    Output:
C!      reordered PIDI bank
C!      IERR  /I  : error flag:
C!                     IERR = 0 if everything was O.K.
C!                     IERR = 1 insufficient space in the BOS array.
C!                                Hence PIDI bank not reordered.
C!                     IERR = 2 if the FICL or PIDI banks did not exist.
C!
C! Reorder the packed digits. bank (PIDI) so that instead of being
C! ordered according to increasing wire number, it is ordered according
C! to which hits have been assigned to which tracks (as given by the
C! FICL bank).
C! The routine IPAKDI is used to create the PIDI bank from the ITDI bank
C!
C?  Check that the FICL and PIDI banks exist.
C?  Loop over the associated coordinates in the FICL bank.
C?    Find the corresponding coordinate numbers in the ITCO (PIDI) banks
C?    Use this to fill the new PIDI bank (adding coord. ambiguity info.)
C?    Note which digitisings are associated with tracks.
C?  End Loop
C?  Loop over digits in PIDI bank looking for unassociated ones.
C?    Place these at the end of the new PIDI bank
C?  End Loop
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
C-----------------------------------------------------------------------
C I/O commons and parameters
#include "bcs.h"
#include "rcurnt.h"
C-----------------------------------------------------------------------
C 'Local' common to store work bank pointers
      COMMON/IDUMYC/IPIDIW,IASDIW
      LOGICAL SETZ
      DATA SETZ/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      IERR = 2
      IF (SETZ) THEN
        IPIDIW = 0
        IASDIW = 0
        SETZ = .FALSE.
      END IF
C
C Get the FICL and PIDI banks.
C
      KFICL = NLINK('FICL',0)
      IF (KFICL.LE.0) GOTO 200
      KPIDI = NLINK('PIDI',0)
      IF (KPIDI.LE.0) GOTO 200
      IERR = 1
C
C Find the total number of associated coordinates and packed digits.
      NUSED = LROWS(KFICL)
      NDIG  = LROWS(KPIDI)
C
C Create a work bank to contain the reordered digitisings.
      IF (IPIDIW.GT.0) THEN
        CALL WDROP(IW,IPIDIW)
        CALL RERROR('IREOPD',1,'IPIDIW work bank still exists '//
     +  'from earlier. It should have been dropped.')
      END IF
      CALL WBANK(IW,IPIDIW,LMHLEN+NDIG,*100)
      IW(IPIDIW+LMHCOL) = 1
      IW(IPIDIW+LMHROW) = NDIG
C
C Create a work bank to show which digits were associated with tracks.
      IF (IASDIW.GT.0) THEN
        CALL WDROP(IW,IASDIW)
        CALL RERROR('IREOPD',2,'IASDIW WORK BANK STILL EXISTS FROM '//
     +  'earlier. It should have been dropped.')
      END IF
      CALL WBANK(IW,IASDIW,LMHLEN+NDIG,*100)
      IW(IASDIW+LMHCOL) = 1
      IW(IASDIW+LMHROW) = NDIG
C
C-----------------------------------------------------------------------
C Loop over the associated coordinates in the FICL bank.
C
      DO 25 M=1,NUSED
C Find the coord. no in the ITCO (and hence PIDI) bank.
        ICO = IW(KFICL+LMHLEN+M)
C Note that this digit was associated with a track.
        IW(IASDIW+LMHLEN+ABS(ICO)) = 1
C Fill the IPIDIW work bank.
        IW(IPIDIW+LMHLEN+M) = IW(KPIDI+LMHLEN+ABS(ICO))
C Set coordinate ambiguity bit (bit 30) if ICO < 0.
        IF(ICO.LT.0) IW(IPIDIW+LMHLEN+M)=IBSET(IW(IPIDIW+LMHLEN+M),30)
   25 CONTINUE
C
C Add unassociated digits to the end of the IPIDIW bank.
      IOFF = NUSED
      DO 50 N=1,NDIG
        IF (IW(IASDIW+LMHLEN+N).EQ.1) GOTO 50
        IOFF = IOFF + 1
        IW(IPIDIW+LMHLEN+IOFF) = IW(KPIDI+LMHLEN+N)
   50 CONTINUE
C
C Create the reordered PIDI bank from the IPIDIW work bank.
C
      CALL BKFRW(IW,'PIDI',0,IW,IPIDIW,*100)
      IERR=0
      GOTO 200
C
C-----------------------------------------------------------------------
  100 CALL RERROR('IREOPD',3,'No space to create BOS banks.'//
     +' PIDI bank could not be reordered.')
C
C-----------------------------------------------------------------------
  200 CONTINUE
C Drop the work banks.
      IF (IPIDIW.NE.0) CALL WDROP(IW,IPIDIW)
      IF (IASDIW.NE.0) CALL WDROP(IW,IASDIW)
      END
#endif
