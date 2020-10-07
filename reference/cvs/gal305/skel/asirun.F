*DK asirun
      SUBROUTINE ASIRUN
C ----------------------------------------------------------------------
C. - F.RANJARD - 850325
C! read run record, set run status banks, write C-list
C.   Change during a run.
C. - called by   ASREAD, ASPRUN                        from this .HLB
C. - calls       ASRUST, ASRRUN, ASWRTP                from this .HLB
C.               ALBOS                                 from ALEPH lib
C.               BLIST, BDROP, BGARB                   from BOS77
C.
C -----------------------------------------------------
#ifndef DOC
#include "jobcom.h"
#include "bcs.h"
C ----------------------------------------------------------------------
C
C - decode run record if it exists
C
      IF (MGETJO.NE.0) CALL ASRRUN
C
C - Build the Run status banks
C
      CALL ASRUST
C
C - write run record if required
      IF (MSAVJO.NE.0) CALL ASWRTP ('C')
      IF (IPRIJO(14) .EQ. 1) CALL AUBLIS ('C')
C
C - Call USER routine
      CALL USIRUN
C
      RETURN
      END
#endif
