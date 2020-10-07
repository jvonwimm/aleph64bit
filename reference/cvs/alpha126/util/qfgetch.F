      SUBROUTINE QFGET_CH (IFRUN, IFEVT, ILRUN, ILEVT)
C-----------------------------------------------------------------------
CKEY FILL BEAMSPOT / INTERNAL
C
C   Author  S. Wasserbaech  2-5-95
C
C   Returns first and last event numbers for the chunk containing the
C   event IRUN/IEVT specified in the previous call to QFGET_BP.  The
C   outputs are zero if the previous call to QFGET_BP was unsuccessful,
C   or if Monte Carlo is being analyzed.
C
C - Input:
C   None.
C
C - Output:
C   IFRUN  / I  Run number for first event in chunk
C   IFEVT  / I  Event number for first event in chunk
C   ILRUN  / I  Run number for last event in chunk
C   ILEVT  / I  Event number for last event in chunk.  Notice that this
C                 does not necessarily correspond to a real event: an
C                 arbitrary large number may be given if the chunk
C                 extends to the end of a run.
C-----------------------------------------------------------------------
#ifndef DOC
      COMMON / GETBPCO / IFRUN0, IFEVT0, ILRUN0, ILEVT0,
     >                   ISETUP0, SIGMAX0
C-----------------------------------------------------------------------
      IFRUN = IFRUN0
      IFEVT = IFEVT0
      ILRUN = ILRUN0
      ILEVT = ILEVT0
C
      RETURN
      END
#endif

