C
      COMMON /TWOINC/IRELHT,IRELSI,TWOSEC(36),ITSORT
      LOGICAL TWOSEC
#if defined(DOC)
C
C! Include for TPC overlapping track dE/dx calculation
C IRELHT = Workbank index of list of shared hits
C IRELSI = Workbank index of list of unshared hits on same wire
C TWOSEC = Logical flag for the sectors in which there are shared hits
C ITSORT = Workbank index for sorting dE/dx samples
C
#endif
