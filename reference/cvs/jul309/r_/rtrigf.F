      SUBROUTINE RTRIGF(IRUN,TLCAL,TRAND,TPHYS,TSATR,FAIL)
C-------------------------------------------------------------
C!  - Return trigger type for current event
C!
C!    Author J.Knobloch 9-MAR-1990
C!    Modified:   H. Meinhard       23-Aug-1990  (1)
C!    Modified :- E. Lancon             10-APR-1991
C!                      Use ALTRIG to access Trigger bits
C!    Input:  IRUN
C!    Output: Tlcal - Trigger is any LCAL trigger (incl SATR trigger)
C!            TRAND - Trigger is random   trigger
C!            TPHYS - Trigger is any physics trigger
C!            TSATR - Trigger is SATR coincidence trigger
C!            FAIL  - no trigger info
C?
C!======================================================================
#ifndef DOC
      LOGICAL TLCAL,TRAND,TPHYS,TSATR,FAIL
      INTEGER ALTRIG
      EXTERNAL ALTRIG
#include "bcs.h"
      DATA LRUN/-99999/
C-----------------------------------------------------------------
      FAIL = .TRUE.
      NTRBIT=0
C
C     set up trigger masks if new run
C
      IF(LRUN.NE.IRUN)THEN
        CALL RTMASK(IRUN,MASKL,MASKR,MASKP,MASKS)
        LRUN=IRUN
      ENDIF
C
C?      Get Trigger Bit Pattern
C
      IF( ALTRIG(IT1,IT2,IL2).GT.0 )THEN
        FAIL=.FALSE.
        NTRBIT=IL2
      ENDIF
C
      TLCAL=IAND(NTRBIT,MASKL).NE.0
      TRAND=IAND(NTRBIT,MASKR).NE.0
      TPHYS=IAND(NTRBIT,MASKP).NE.0
      TSATR=IAND(NTRBIT,MASKS).NE.0
C
  999 RETURN
      END
#endif
