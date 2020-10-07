*DK itirun
      SUBROUTINE ITIRUN
C.
C...ITIRUN  1.18  930721  18:08                        R.Beuselinck
C.
C!  Initialise ITC routines.
C.
C.  Define formats for all banks used and set /ITNAMC/.
C.  Initialise ITC readout geometry and calibration from database.
C.
C.  Called by: ASIPAC                                   from this .HLB
C.      Calls: ITWINI, ITDCNS, ITRGPA                   from this .HLB
C.             NAMIND, NLIST, BKFMT, BLIST              from BOS77
C.             ALTELL, ALGTDB                           from ALEPHLIB
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "iocom.h"
#include "jobcom.h"
#include "itnamc.h"
#include "itsumc.h"
C
      CHARACTER*(*) LIST, NAME*4
      PARAMETER (LIST =
     +  'IGEOITCCILYRIALIIRFEIZFEIZNLIZRSIDRPIRRFIXBWIXCRIEFF')
C
      INTEGER  ALGTDB, GTSTUP
      CHARACTER*4 NLIST
      EXTERNAL NLIST, ALGTDB, GTSTUP
C
C--  Check ITC run condition parameter for legal values
C--
      IF (ICITJO(1).LT.0 .OR. ICITJO(1).GT.2) THEN
         CALL ALTELL('ITIRUN: bad values for run conditions ',0,'STOP')
      ENDIF
C
      NAIHIT = NAMIND('IHIT')
      NAITTR = NAMIND('ITTR')
      NAIDIG = NAMIND('IDIG')
      NAIDHR = NAMIND('IDHR')
      NAIXBW = NAMIND('IXBW')
C
      CALL BKFMT('IHIT','2I,(3I,5F)')
      CALL BKFMT('IDIG','2I,(I)')
      CALL BKFMT('ITTR','2I,(I)')
      CALL BKFMT('IXRP','2I,(I)')
      CALL BKFMT('IDHR','2I,(I)')
C
      JDITWP = 0
      JDITFN = 0
      JDITFP = 0
      JDITNW = 0
      JDITAB = 0
      JDITDC = 0
      JDIZSC = 0
C
      IRUN = GTSTUP('IT',IRUNJO)
      IF (IRUN.EQ.-1) THEN
C       ... no setup number found. ==> Fatal error.
        CALL ALTELL('ITIRUN: no setup found from GTSTUP',0,'STOP')
      ENDIF
C
C--   Load the banks needed from the Data Base one at a time to check
C--   each one for updating or error.  If you don't care then a single
C--   call to ALGTDB with LIST instead of NAME will do.
C--
      N = 1
   10 NAME = NLIST(IW,N,LIST)
      IF (NAME.NE.'    ') THEN
        IND = ALGTDB(LRDBIO, NAME, IRUN)
        IF (IND.EQ.0) THEN
          CALL ALTELL('ITIRUN: Missing Data Base bank '//NAME//
     +  ' for ITC',0,'STOP')
        ENDIF
        N = N + 1
        GO TO 10
      ENDIF
C
C--  Initialise ITC readout geometry.
C--
      CALL ITWINI
C
C--  Setup drift time constants.
C--
      CALL ITDCNS
C
C--  Initialise trigger constants.
C--
      CALL ITRGPA
C
C--  Initialise alignment constants.
C--
      CALL ITROTI
C
C--  Print geometry if required
C
      IF (IPRIJO(2) .NE. 0) CALL ITPGEO
C
C--  Initialise summary counters.
C--
      NEVHIT = 0
      NEVDIT = 0
      DO 20 I=1,10
        NTSMIT(I) = 0
        NHSMIT(I) = 0
        NDSMIT(I) = 0
        NDHSIT(I) = 0
   20 CONTINUE
      END
#endif
