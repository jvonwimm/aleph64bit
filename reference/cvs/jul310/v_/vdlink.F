      SUBROUTINE  VDLINK
C----------------------------------------------------------------------
C!  - steering routine for MVD hit association and track refit
C!
C!    Author :- hans-guenther moser         25/01/88
C!
C!    last modification: 4-2-91 David Brown
C!    Rewritten to drive the new MOURS vertex refitting
C!    Modified A. Bonissent May 1995
C!    Compress Monte Carlo truth information
C!
C?
C!======================================================================
#ifndef DOC
#include "vprtnc.h"
#include "rconds.h"
#include "vrecon.h"
C
C  Local variables
C
      INTEGER IER,IGARB
      INTEGER NAMIND
      INTEGER NMATC(2)
      REAL FIELRC,ENERRC
#include "bcs.h"
C
C  First, unpack the pot banks.  This is necessary for debugging
C  with old data.  This will happen ONLY if the bank VUNP appears
C
      IF(IW(NAMIND('VUNP')).GT.0)THEN
        CALL AUNPCK('E','IT TP FI SO ',IER)
      END IF
C
C  TRACK EXTRAPOLATION
C
      CALL VTRKEX(FIELRC,IER)
      IF(IER.EQ.1) CALL RERROR('VTRKEX',IER,
     &  ' Iterations step too large')
      IF(IER.EQ.2) CALL RERROR('VTRKEX',IER,
     &  ' Iterations not converging')
      IF(IER.EQ.3) CALL RERROR('VTRKEX',IER,
     &  ' Iteration result not consistant')
      IF(IER.EQ.4) CALL RERROR('VTRKEX',IER,
     &  ' Cylinder extrapolation failed')
      IF(IER.EQ.5) CALL RERROR('VTRKEX',IER,
     &  ' No FRFT or FRTL banks')
      IF(IER.EQ.6) CALL RERROR('VTRKEX',IER,
     &  ' Not enough BOS space')
C
C   Do VDET track refit, with errors calculated at the origin
C
      CALL VTRFIT(1,FIELRC,IER,IGARB,NMATC)
      IF(IER.EQ.4) CALL RERROR('VTRFIT',IER,
     &  ' not enough work bank space')
      IF(IER.EQ.5) CALL RERROR('VTRFIT',IER,
     &  ' not enough name bank space')
      IF(IER.EQ.6) CALL RERROR('VTRFIT',IER,
     &  ' missing some named bank')
      IF(IER.EQ.11 .OR. IER .EQ. 12 .OR. IER .EQ. 14)
     &  CALL RERROR('VTRFIT',IER,'not enough name bank space')
C
C  Increment the run summary matched hit count
C
      IF(IER.EQ.0)THEN
        NMATHT(1) = NMATHT(1) + NMATC(1)
        NMATHT(2) = NMATHT(2) + NMATC(2)
      END IF
C
C  Compress Monte Carlo truth cluster to FKIN tracks relation
C
      IF(IER.EQ.0.AND.MCEVNT)THEN
        CALL VDMCTR(IERV)
        IF(IERV.NE.0)
     >   CALL RERROR('VDLINK',1,'Bad return code from VDMCTR')
      END IF
      RETURN
      END
#endif
