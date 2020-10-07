      PARAMETER (MRSTA=7,MRERR=9)
      COMMON /RSTATR/NRUNRS(MRSTA),NRUNRR(MRERR)
      INTEGER NRUNRS,NRUNRR
#if defined(DOC)
C
C-------> STATISTICS AND ERRORS FOR THE RUNS
C
C MRSTA   = Number of run statistic classes
C MRERR   = Number of run error classes
C
C NRUNRS(1) = Total runs processed in this job
C NRUNRS(2) = Runs written to OUTPUT
C NRUNRS(3) = Runs summarised in database
C NRUNRS(4) = Total events processed in this job
C NRUNRS(5) = Events written to OUTPUT
C NRUNRS(6) = Average reconstruction time per event (msecs)
C NRUNRS(7) = Average event size (KBytes)
C
C NRUNRR(1) = Runs that failed for any reason
C NRUNRR(2) = Runs not processed due to absence of constants
C NRUNRR(3) = Runs not processed due to bad constants
C NRUNRR(4) = Runs not in log book
C NRUNRR(5) = Runs not on given tape
C NRUNRR(6) = Total events with bad header this job
C NRUNRR(7) = Total events absent on input this job
C NRUNRR(8) = Total events rejected in <prepare data>
C NRUNRR(9) = Total events rejected in <reconstruct event>
C
C
#endif
