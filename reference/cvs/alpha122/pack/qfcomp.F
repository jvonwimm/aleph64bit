      SUBROUTINE QFCOMP (NTKE, NTKH, FLAG)
CKEY FILL CAL /INTERNAL
C----------------------------------------------------------------------
C! Decide whether ECAL object NTKE and HCAL object NTKH really form a
C! composite object. Called once for each ECAL object which is
C! associated to an HCAL object.
C! Output : Set FLAG to .FALSE. and the relation between ECAL and HCAL
C! will be ignored.
C!
C called from QFCREL
C!                                                   H.Albrecht 27.2.89
C----------------------------------------------------------------------
#ifndef DOC
      LOGICAL FLAG
#include "qcde.h"
#include "qmacro.h"
C----------------------------------------------------------------------
C
      FLAG = .TRUE.
C
C       Number of associated charged tracks :
C
      NCHGDE = KNCHGD(NTKE)
      NCHGDH = KNCHGD(NTKH)
C
C       relation ok if both objects are ass. to a charged track :
C
      IF (NCHGDE .GT. 0 .AND. NCHGDH .GT. 0)  GO TO 90
C
C       relation ok if both objects are NOT ass. to a charged track :
C
      IF (NCHGDE .EQ. 0 .AND. NCHGDH .EQ. 0)  GO TO 90
C
      IF (NCHGDE .GT. 0)  THEN
C         ECAL is assoc. and HCAL is NOT :
        FLAG = .FALSE.
      ELSE
C         HCAL is assoc. and ECAL is NOT :
C       IF (QLIKEH(NTKE,KHPHOT) .GT. 0.1)  FLAG = .FALSE.
      ENDIF
C
   90 RETURN
      END
#endif
