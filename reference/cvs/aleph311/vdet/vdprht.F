      SUBROUTINE VDPRHT
C
C... VDPRHT  1.00   860611                       F. Forti
C
C! Print out  'VDHT' bank in readable format.
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "bmacro.h"
C -------------------------------------------------------------------
C
      LOUT = IW(6)
C
      NR = 0
      IND = NLINK ('VDHT',0)
      IF (IND.GT.0) THEN
         LW = LCOLS (IND)
         LHIT = LROWS (IND)
         WRITE (LOUT,1000) LHIT,LW
         KND = IND + LMHLEN
         DO 100 IH=1,LHIT
         WRITE (LOUT,1010) (IW(KND+I),I=1,3),(RW(KND+J),J=4,LW)
 100     KND = KND + LW
      ENDIF
 1000 FORMAT(//' +++VDPRHT+++ VDHT bank printout'/
     &        '    Number of hits :',I4,
     &        '   Number of words per hit :',I4//
     &        6X,'Track  Layer  Phi        XIN      YIN      ZIN',
     &        6X,' XOUT     YOUT     ZOUT      ENERGY')
 1010 FORMAT(7X,I4,6X ,I1,  3X,I2 ,2(2X,3(2X,F7.3)),4X,E8.2,(G8.3))
  999 CONTINUE
      RETURN
      END
#endif
