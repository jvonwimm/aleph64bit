*CD itexpc
      COMMON/ITEXPC/CLOKIT,CLGOIT,TSTRIT,TBINIT,PLENIT(8),EXPFIT(8)
C
#if defined(DOC)
   Constants for ITC time-expansion routines  (Z measurement).
        CLOKIT = clock pulse interval for Z coincidence test (ns).
        CLGOIT = random clock start w.r.t. time of event (ns).
        TSTRIT = start time for calculation of ITC theta bin (ns).
        TBINIT = time width of one ITC theta bin (ns).
        PLENIT = pulse lengths for Z coincidence circuit (ns).
        EXPFIT = expansion factor for each layer.
#endif
