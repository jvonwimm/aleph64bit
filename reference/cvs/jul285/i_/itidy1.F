      SUBROUTINE ITIDY1 (CUT1,IFLAG)
C
C------------------------------------------------------------------------------
C!    Ensure that a TPC point is assigned to only one ITC track
C!
C!    Author         M J Phillips with J Sedgbeer       01-Jan-1991
C!
C!    Input          CUT1        Maximum acceptable value of a
C!                               entry in bank IDSQ for a good
C!                               track-point combination
C!    Output         IFLAG = 0   Successful
C!                           1   No IDSQ bank found
C!                           2   No TPC points
C!                           3   No ITC tracks
C!
C!    Description
C!    ===========
C!    Each TPC point must be associated with no more than one ITC
C!    track. The bank IDSQ is amended if this is not the case.
C!
C?    The bank IDSQ (which contains the quantity DSQD for each point-
C?    track combination) is scanned. We ensure that each (TPC) point
C?    is only (preliminarily) assigned to one (ITC) track, by increasing
C?    DSQD of any other point-track combinations (which currently have
C?    DSQD < CUT1) by an amount CUT1.
C?
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
      REAL CUT1
      EXTERNAL NLINK
#include "bmacro.h"
      IFLAG = 0
      KIDSQ = NLINK ('IDSQ',0)
           IF (KIDSQ.EQ.0) THEN
               IFLAG = 1
               GOTO 250
           ENDIF
      NTPT = LCOLS (KIDSQ)
      NTRK = LROWS (KIDSQ)
           IF (NTPT.EQ.0) THEN
               IFLAG = 2
               GOTO 250
           ENDIF
           IF (NTRK.EQ.0) THEN
               IFLAG = 3
               GOTO 250
           ENDIF
C
C  Look at each TPC point
C
      DO 200 M = 1, NTPT
C
C  ICNT will be the track-number of the best ITC track to which
C  this TPC point can be assigned.
C  DBEST will be the value of DSQD (entry in the bank IDSQ) for this
C  (i.e. the best) track-point combination.
C
            ICNT = 0
            DBEST = 999999999999.
C
C  Look at each ITC track
C
            DO 150 I = 1, NTRK
C
C  Find DSQD (=entry in bank IDSQ) for this point-track combination
C
                  DSQD = RTABL (KIDSQ,I,M)
                  IF (DSQD.LT.0.0) GOTO 150
                  IF (DSQD.GT.CUT1) GOTO 150
C
C  If worse than "best-so-far", then set DSQD for this combination
C  to be greater than CUT1.
C
                  IF (DSQD.GT.DBEST) THEN
                        RW (KIDSQ + LMHLEN + (I-1)*NTPT + M) =
     +                  CUT1 + RTABL (KIDSQ,I,M)
                        GOTO 150
                  ENDIF
C
C  Else set DSQD for the previous "best" to be greater than CUT1, unless
C  this is the first candidate track-point combination for this
C  particular point to be considered.
C
                  IF (ICNT.NE.0)
     +                  RW(KIDSQ + LMHLEN + (ICNT-1)*NTPT + M) =
     +                  CUT1 + RTABL (KIDSQ,ICNT,M)
C
C  This point-track combination becomes the new "best".
C
                  ICNT = I
                  DBEST = DSQD
 150        CONTINUE
 200  CONTINUE
 250  RETURN
      END
#endif
