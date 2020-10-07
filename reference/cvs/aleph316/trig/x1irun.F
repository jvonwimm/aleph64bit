      SUBROUTINE X1IRUN(IRUN)
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 95/03/20  FOR ALEPHLIB 205
C.              modified B.Bloch may 96 for setup dependent thresholds
C.
C! - Initialize the Level1 Trigger Part of GALEPH
C.
C. - Banks    : X1TV (Trigger thresholds taken from data base)
C.              Bank formats defined for trigger output banks
C.
#ifndef DOC
      SAVE
      EXTERNAL NAMIND ,GTSTUP,AGETDB
      INTEGER AGETDB ,GTSTUP
#include "bcs.h"
#include "x1tcom.h"
#include "x1namc.h"
C.
C ----------------------------------------------------------------------
C.
C.  -  Reset Level1 Physics Trigger counters
C.
      DO 98 I = 1,NPHTR
 98   NPHYTR(I) = 0
      NAX1TV = NAMIND ('X1TV')

C.  -  Get Sical thresholds if needed and trigger related constants
      MCRUN = IRUN
      ISTUP = GTSTUP ('SI',MCRUN)
      IF ( ISTUP.GT.0) THEN
         JND = AGETDB('SITCSRCO',ISTUP)
         IF (JND.NE.0) GO TO 97
         WRITE(IW(6),990)
 990  FORMAT(////,'  Warning No SICAL Thresholds found for this run  ',
     $       /,'  Trigger will be applied using defaults settings',
     $       ///)
 97      CONTINUE
         CALL SITRIN
      ENDIF
C.
C.
C.  -  Get table containing trigger thresholds for this run
C.     using a setup dependent scheme
C.
C.
      ISTUP = GTSTUP ('TR',MCRUN)
      IF ( ISTUP.GT.0) THEN
          IND = AGETDB('X1TV',ISTUP)
          IF (IND.NE.0) GO TO 99
      ENDIF
      WRITE(IW(6),991)
 991  FORMAT(////,'  Warning No Thresholds found for this run; ',
     X          /,'  Trigger cannot be applied!!!!',
     X          /,'  Therefore the job will be stopped.',
     X          /,'  Resubmit without requiring the trigger'
     X          /,'  part or provide the bank X1TV',///)
      CALL ALTELL('X1IRUN Missing bank X1TV',0,'STOP')
      GO TO 999
  99  CONTINUE
      DO 100 I = 1,NPHTR
 100  NPHYTR(I) = 0
C
C - Define bank formats
C
      CALL BKFMT('X1IP','2I,(A,36I)')
      NASIXA = NAMIND ('SIXA')
      CALL BKFMT('SIXA','I')
      NASIX2 = NAMIND ('SIX2')
      CALL BKFMT('SIX2','I')
      NASIFO = NAMIND ('SIFO')
      CALL BKFMT('SIFO','I')
C
 999  CONTINUE
      RETURN
      END
#endif
