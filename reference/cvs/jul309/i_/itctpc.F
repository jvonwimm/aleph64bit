      SUBROUTINE ITCTPC
C-----------------------------------------------------------------------
C!   ITC-TPC tracking - Steering.
C!
C!    Author      :- J. Sedgbeer 87/02/09
C!    Modified    :- J. Sedgbeer 88/06/22
C!    Modified    :- I. Tomalin  88/11/17
C!
C!   Input:
C!       /BCS/ and /IWBNKT/ for work bank IWTPCP
C!
C!   Output: None
C!
C!   called by : ITCREC
C!
C!   calls :  IGETTI - Get TPC tracks initialisation
C!            IGETTP - Get TPC track parameters
C!            IGICHA - Get ITC chain ICHA
C!
C!   Libraries required: none
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C I/O Commons etc.
#include "bcs.h"
#include "iwbnkt.h"
C-----------------------------------------------------------------------
      LOGICAL SPIRAL,OUT,IGOOD,MORE
      DIMENSION PAR(6),COV(21)
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialise TPC Track selection
C
        CALL IGETTI(IER)
        IF (IER.GT.0) GOTO 999
C
        NTRK = LROWS(IWTPCP)
        IF(NTRK.LE.0) THEN
           IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) WRITE(LDEBRL,1000)
 1000      FORMAT(1H0,'ITCTPC: No suitable TPC tracks for extension')
           GOTO 999
        ENDIF
C
C Loop over TPC tracks
C
      DO 100 I=1,NTRK
          INTPC = IW(IWTPCP+LMHLEN+I)
C First try extending the 1st arcs of the TPC tracks.
C Then try extending the following spirals.
          SPIRAL=.FALSE.
   10     CONTINUE
C Set up TPC track parameters.
          CALL IGETTP(INTPC,SPIRAL,PAR,COV,OUT,DSPIR,IGOOD,MORE)
C
C Use TPC Track parameters to search for chain in ITC.
C
          IF (IGOOD) CALL IGICHA(PAR,COV,SPIRAL,OUT,DSPIR)
C See if this track spirals any further.
          IF (.NOT.MORE) GOTO 100
C
          SPIRAL=.TRUE.
          GOTO 10
C
  100 CONTINUE
C
  999 RETURN
      END
#endif
