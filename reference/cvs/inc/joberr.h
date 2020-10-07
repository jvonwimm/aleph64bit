*CD joberr
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      INTEGER ITELJO, KERRJO, NERRJO
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
#if defined(DOC)
      ITELJO         =  1 if TAXION = STOP, END or FATAL
      KERRJO         =  error code
      NERRJO(1-lerr) =  error counters
      TACTJO         =  action to be taken: RETURN, NEXT, STOP, END, FATAL
                                            INIT
#endif
