*DK galeph
      PROGRAM GALEPH
C ----------------------------------------------------------------------
C! MonteCarlo main routine
C. - F.RANJARD - 850222
#include "version.h"
C ---------------------------------------------------------------------
#ifndef DOC
#if defined(INTER)
      PARAMETER (LGB = 920000, LHB = 50000)
#else
      PARAMETER (LGB = 700000, LHB = 20000)
#endif
      COMMON /PAWC/   HB(LHB)
      COMMON /GCBANK/   GB(LGB)
C
C
C ----------------------------------------------------------------------
#if defined(INTER)
      CALL GPAW (LGB,LHB)
#else
C - call TIMEST to initialize job time limit , MANDATORY on VAX when
C   running interactively, dummy routine on other machines and in BATCH
      CALL TIMEST (9999999.)
C
C - Initialize blank common for HBOOK and /GCBANK/ for GEANT3
C
      CALL GZEBRA (LGB)
      CALL HLIMIT (-LHB)
C
C - GALEPH initialization
C
      CALL ASIGAL
C
C - Process the run : loop over events
C   Then close the run and the job if time limit or last event
C   or end of run or end of file
C
      CALL QNEXTE
C
#endif
      STOP
      END
#endif
