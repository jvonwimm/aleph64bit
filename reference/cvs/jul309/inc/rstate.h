      PARAMETER (MESTA=3,MEERR=5)
      COMMON /RSTATE/NEVTRS(MESTA),NEVTRR(MEERR)
      INTEGER NEVTRS,NEVTRR
#if defined(DOC)
C
C-------> STATISTICS AND ERRORS FOR THE EVENTS
C
C MESTA   = Number of event statistic classes
C MEERR   = Number of event error classes
C
C NEVTRS(1) = Total events processed in this run
C NEVTRS(2) = Events written to OUTPUT
C NEVTRS(3) = Average reconstruction time per event (msecs)
C
C NEVTRR(1) = Total events that failed for any reason
C NEVTRR(2) = Events rejected before <prepare data>
C NEVTRR(3) = Events absent on input file
C NEVTRR(4) = Events rejected in <prepare data>
C NEVTRR(5) = Events rejected in <reconstruct event>
C
C
#endif
