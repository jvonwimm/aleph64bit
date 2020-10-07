C!    Current event and event numbers
      LOGICAL INIRUC, FMCRUN
      INTEGER IRUNRC,IEVTRC,ISQNRC
      REAL RENLEP
      COMMON /RCURNT/IRUNRC,IEVTRC,ISQNRC, INIRUC, FMCRUN, RENLEP
#if defined(DOC)
C IRUNRC  = The current run number
C IEVTRC  = The current TRIGGER number (the event number on tape)
C ISQNRC  = The number of the event counted from the start
C INIRUC  = .TRUE. if run has been initialised
C FMCRUN  = .TRUE. if MC run
C RENLEN  = LEP c.m. Energy in GeV
C
#endif
