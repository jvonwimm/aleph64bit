      SUBROUTINE ITCREC
C-----------------------------------------------------------------------
C! ITC Reconstruction. Steering routine.
C!
C!    Author      :- J. Sedgbeer 87/02/09
C!    Modified    :- J. Sedgbeer 88/06/22
C!    Modified    :- I. Tomalin  89/02/28
C!    Modified    :- J. Sedgbeer 90/01/06
C!
C!   Input:
C!      commons:     /BCS/
C!                   /ITRKPA/ for tracking cuts
C!                   /RCURNT/ for run and event number
C!                   /IFLAGS/ for tracking options
C!
C!   Output:
C!      commons:     /ITTIME/
C!
C!   called by : RECEVT
C!
C!   calls :
C!     ITRECI  Initialise ITC-TPC tracking
C!     ITCTPC  Find ITC tracks by extending existing TPC tracks
C!     ITRECF  Finalise ITC-TPC tracking
C!     ITCTRK  ITC stand-alone tracking
C!     IMATCH  Match unextended TPC and ITC stand-alone tracks
C!     ITDCOR  Create ITDR and ITDL banks
C!     ITRMCA  Associate the JULIA ITC tracks with the true MC ones
C!
C!   Libraries required: none
C!
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
#include "bosext.h"
C-----------------------------------------------------------------------
C I/O Commons etc.
#include "bcs.h"
#include "itrkpa.h"
#include "rcurnt.h"
#include "ittime.h"
#include "iflags.h"
C-----------------------------------------------------------------------
C
C Use TPC tracks to find ITC tracks. Initialise; Find; Finalise.
C
      IF( IFITTI ) CALL ALTIME ( TIME1 )
      CALL ITRECI(IER)
      IF( IFITTI ) CALL ALTIME ( TIME2 )
      TTRECI=TTRECI+(TIME2-TIME1)
      IF (IER.EQ.0) THEN
        IF (FTPCIF) THEN
        IF( IFITTI ) CALL ALTIME ( TIME1 )
          CALL ITCTPC
        IF( IFITTI ) CALL ALTIME ( TIME2 )
          TTCTPC=TTCTPC+(TIME2-TIME1)
        ENDIF
      ENDIF
      IF (IER.LE.1) THEN
        IF( IFITTI ) CALL ALTIME ( TIME1 )
        CALL ITRECF
        IF( IFITTI ) CALL ALTIME ( TIME2 )
        TTRECF=TTRECF+(TIME2-TIME1)
      ENDIF
C
C ITC (alone) tracking. (Do this even in TGFT bank was missing).
C
      IF (IER.EQ.0.OR.IER.EQ.2) THEN
        IF (FITCIF) THEN
          IF( IFITTI ) CALL ALTIME ( TIME1 )
          CALL ITCTRK
          IF( IFITTI ) CALL ALTIME ( TIME2 )
          TTCTRK=TTCTRK+(TIME2-TIME1)
        ENDIF
C
C Try to match non-extended TPC tracks with ITC stand-alone tracks.
        IF (FMATIF) THEN
          IF( IFITTI ) CALL ALTIME ( TIME1 )
          IF (IER.EQ.0) CALL IMATCH
          IF( IFITTI ) CALL ALTIME ( TIME2 )
          TMATCH=TMATCH+(TIME2-TIME1)
        ENDIF
C
C Extend ITC alone tracks out to TPC (try to add TPC hits)
        IF (FEXTIF) CALL ITEXTN
C
C Create corrected coord. bank for final fit (UFTTRK)
        CALL ICRCCO
C
C Refine/improve the already found tracks
        IF (FREFIF) CALL IREFIN
C
C Associate the JULIA ITC tracks with the true MC ones.
C   First create the ITDR and ITDL banks and add them to the T list.
        IF (FMCAIF) THEN
          CALL ITDCOR
          CALL BLIST(IW,'T+','ITDRITDL')
          IF( IFITTI ) CALL ALTIME ( TIME1 )
          CALL ITRMCA
          IF( IFITTI ) CALL ALTIME ( TIME2 )
          TTRMCA=TTRMCA+(TIME2-TIME1)
        ENDIF
C
      ENDIF
      END
#endif
