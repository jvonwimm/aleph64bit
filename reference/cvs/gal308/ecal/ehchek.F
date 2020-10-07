*DK ehchek
      SUBROUTINE EHCHEK(NST,DTB,PTB,PV,MFLSL)
C.----------------------------------------------------------------
C R.CLIFFT
C! Parametrisation allowed ?
C *** TEST IF DISTANCE IN MODULE ALLOWS PARAMETRISATION OR NOT
C
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "agcons.h"
#include "ehfcon.h"
C
      MFLSL = 0
      PVM=PV*1000.
      RINT=PI/9
      IF (DTB .LT. 1.E-10) RETURN
      ATH = PTB/DTB
      IF (ABS(ATH) .GT. 1.) ATH = SIGN(1.,ATH)
      TH = ACOS(ATH)
      GO TO (10,20,30,40) ,NST
C
C *** BARREL STACKS1/2
C
 10   RANGE=CST1EH(3)*(CST1EH(1)*ALOG(PVM)-CST1EH(2))
      IF (RANGE.GT.DTB) RETURN
      GO TO 50
C
C ***  END CAP STACKS1/2
C
 20   RANGE=CST1EH(3)*(CST1EH(1)*ALOG(PVM)-CST1EH(2))
      IF (RANGE.GT.DTB) RETURN
      GO TO 50
C
C *** BARREL STACK3
C
 30   N=1.+TH/RINT
      IF(TH.GT.((N-1)*RINT+N*RINT)/2)  THEN
      NREF=N+1
      ELSE
      NREF=N
      ENDIF
      GO TO (32,34,36,38,38),NREF
 32   DD=CBS3EH(1)*ALOG(PVM)-CBS3EH(5)
      GO TO 39
 34   DD=CBS3EH(2)*ALOG(PVM)-CBS3EH(6)
      GO TO 39
 36   DD=CBS3EH(3)*ALOG(PVM)-CBS3EH(7)
      GO TO 39
 38   DD=CBS3EH(4)*ALOG(PVM)-CBS3EH(8)
C
 39   RANGE=CBS3EH(9)*DD/COS(RINT*(NREF-1))
      IF (RANGE.GT.DTB) RETURN
      GO TO 50
C
C *** END CAP STACK3
C
 40   N=1.+TH/RINT
      IF(TH.GT.((N-1)*RINT+N*RINT)/2)  THEN
      NREF=N+1
      ELSE
      NREF=N
      ENDIF
      GO TO (42,44,46,48,48),NREF
 42   DD=CCS3EH(1)*ALOG(PVM)-CCS3EH(5)
      GO TO 49
 44   DD=CCS3EH(2)*ALOG(PVM)-CCS3EH(6)
      GO TO 49
 46   DD=CCS3EH(3)*ALOG(PVM)-CCS3EH(7)
      GO TO 49
 48   DD=CCS3EH(4)*ALOG(PVM)-CCS3EH(8)
C
 49   RANGE=CCS3EH(9)*DD/COS(RINT*(NREF-1))
      IF (RANGE.GT.DTB) RETURN
C
C *** WE CONTAIN SHOWER SO SET FLAG
C
 50   MFLSL=1
C
      RETURN
      END
#endif
