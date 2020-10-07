*CD itsumc
      COMMON/ITSUMC/NEVHIT,NEVDIT,NTCUIT,NHCUIT,NDCUIT,DIGHIT,
     +      NTSMIT(10),NHSMIT(10),NDSMIT(10),NDHSIT(10)
      INTEGER NEVHIT,NEVDIT,NTCUIT,NHCUIT,NDCUIT,
     +      NTSMIT,NHSMIT,NDSMIT,NDHSIT
      REAL DIGHIT
C
#if defined(DOC)
      ITC run summary variables.
      NEVHIT       = Number of events with ITC hits generated.
      NEVDIT       = Number of events with ITC digits generated.
      NTCUIT       = Number of ITC track elements in current event.
      NHCUIT       = Number of ITC hits in current event.
      NDCUIT       = Number of ITC digits in current event.
      DIGHIT       = Fraction of (# digits)/(# hits) in current event.
      NTSMIT(10)   = Pseudo histogram of track elements.
      NHSMIT(10)   = Pseudo histogram of hits.
      NDSMIT(10)   = Pseudo histogram of digits.
      NDHSIT(10)   = Pseudo histogram of DIGHIT.
#endif
