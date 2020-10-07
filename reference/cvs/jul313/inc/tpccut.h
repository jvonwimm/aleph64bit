      COMMON/TPCCUT/ IALGTP,ALGPTP,NPHPTP,CRTLTP,CMNPTP,RTMXTP,
     &               RLTHTP,IMXPTP
#if defined(DOC)
C
C!       Cuts for TPC-coordinate determination.  Loaded in TRNCON.
C
C  IALGTP = Indicates algorithm(s) to use for coordinate finding.
C  ALGPTP = REAL parameter, for use in coordinate algorithms.
C  NPHPTP = Max. number of pads in cluster if 2 half pads included
C  CRTLTP = Limit on how far out side of cluster the coordinate can be
C  CMNPTP = Used to cut out low pulse-height pulses in a cluster
C  RTMXTP = Cut on ratio of pulse heights 1 and 3 in 3-pad cluster
C  RLTHTP = Cut on ratio of pulse height relative to cluster maximum
C  IMXPTP = Don't bother trimming clusters larger than this
C---------------------------------------------------------------------
#endif
