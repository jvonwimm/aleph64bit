      SUBROUTINE ITIDY2 (CUT1,IFLAG)
C
C-------------------------------------------------------------------------------
C!    Only one TPC point with a given r-coord can be added to an ITC track
C!
C!    Author         M J Phillips with J Sedgbeer      01-Jan-1991
C!
C!    Input          CUT1        Maximum acceptable value of an
C!                               entry in bank IDSQ for a good
C!                               track-point combination
C!    Output         IFLAG = 0   Successful
C!                           1   No IDSQ bank present
C!                           2   No TPC points
C!                           3   No ITC tracks
C!                           4   No TPCO bank
C!
C!    Description
C!    ===========
C!    Look at the set of TPC points which are candidates for adding onto
C!    an ITC track. If more than one point has the same r-coordinate, then
C!    keep only the point with the lowest DSQD entry in the bank IDSQ.
C?
C?    For a given track, the entry, DSQD, for all track-point combinations
C?    apart from the best (i.e. smallest) one, is increased by an
C?    amount CUT1.
C?
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "tpcojj.h"
      REAL CUT1
#include "bmacro.h"
      IFLAG = 0
      KIDSQ = NLINK ('IDSQ',0)
      IF (KIDSQ.EQ.0) THEN
           IFLAG = 1
           GOTO 350
      ENDIF
      NTRK = LROWS (KIDSQ)
      IF (NTRK.EQ.0) THEN
           IFLAG = 3
           GOTO 350
      ENDIF
      NTPT = LCOLS (KIDSQ)
      IF (NTPT.EQ.0) THEN
           IFLAG = 2
           GOTO 350
      ENDIF
      KTPCO = NLINK ('TPCO',0)
      IF (KTPCO.EQ.0) THEN
           IFLAG = 4
           GOTO 350
      ENDIF
C
C  Look at each ITC track
C
      DO 300 I = 1, NTRK
C
C  Look at each TPC point
C
            DO 250 M = 1, NTPT
C
C  Get DSQD (=entry in bank IDSQ) for this point-track combination,
C  and check that it is a candidate combination.
C
                  DSQD = RTABL (KIDSQ,I,M)
                  IF (DSQD.LT.0.0) GOTO 250
                  IF (DSQD.GT.CUT1) GOTO 250
C
C  RCURR is r-coordinate of current point
C
                  RCURR = RTABL (KTPCO,M,JTPCRV)
C
C  Look at all previous points
C
                  DO 230 N = 1, M-1
C
C  Get DSQD for previous point-track combination,
C  and check that it was a candidate combination.
C
                        DSQD1 = RTABL (KIDSQ,I,N)
                        IF (DSQD1.LT.0.0) GOTO 230
                        IF (DSQD1.GT.CUT1) GOTO 230
C
C  RTEMP is r-coordinate of previous point
C
                        RTEMP = RTABL (KTPCO,N,JTPCRV)
                        IF (RTEMP.NE.RCURR) GOTO 230
C
C  The current point and the previous point both have the same
C  r-coordinate. Indicate that the worse of the two is no longer
C  to be considered by increasing the DSQD (=entry in bank IDSQ)
C  of the worse combination by an amount CUT1.
C
                        IF (DSQD.GT.DSQD1) THEN
                              RW (KIDSQ + 2 + (I-1)*NTPT + M)
     +                      = CUT1 + RTABL (KIDSQ,I,M)
                        ELSE
                              RW (KIDSQ + 2 + (I-1)*NTPT + N)
     +                      = CUT1 + RTABL (KIDSQ,I,M)
                        ENDIF
                        GOTO 250
 230              CONTINUE
 250        CONTINUE
 300  CONTINUE
 350  RETURN
      END
#endif
