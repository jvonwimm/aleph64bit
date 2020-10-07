      SUBROUTINE ITPRHT
C.
C...ITPRHT  2.00  900430  18:00                        R.Beuselinck.
C.
CKEY PRINT ITC MCARLO
C.
C!  Print out contents of ITC hit bank in a readable form.
C.
C.  Input bank: IHIT or ITHT
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "bmacro.h"
C --------------------------------------------------------------------
      LOUT = IW(6)
C
      KIT = NLINK ('IHIT',0)
      IF (KIT.EQ.0) KIT = NLINK ('ITHT',0)
      IF (KIT.EQ.0) GO TO 999
      NH = LROWS (KIT)
      LW = LCOLS (KIT)
      IF (LW.EQ.8) THEN
        WRITE(LOUT,1000) NH
        KK = KIT + LMHLEN
        DO 10 I=1,NH
          WRITE(LOUT,1001,IOSTAT=IOS) I,(IW(KK+J),J=1,LW)
          KK = KK + LW
   10   CONTINUE
      ELSE IF (LW.EQ.9) THEN
        WRITE(LOUT,1002) NH
        KK = KIT + LMHLEN
        DO 20 I=1,NH
          WRITE(LOUT,1003,IOSTAT=IOS) I,(IW(KK+J),J=1,LW)
          KK = KK + LW
   20   CONTINUE
      ENDIF
  999 CONTINUE
 1000 FORMAT(//' IHIT bank.   Number of hits in ITC = ',I4//
     +'  Hit Track layer wire   Dist    Phi         Z',
     +'     Theta       TOF')
 1001 FORMAT(1X,I4,2I6,I5,F7.3,F7.4,3F10.4)
 1002 FORMAT(//' ITHT bank.   Number of hits in ITC = ',I4//
     +'  Hit Track layer wire    Phi      Z   dPhi/dR',
     +'     dZ/dR  Phi(h-w)       TOF')
 1003 FORMAT(1X,I4,2I6,I5,F7.3,F7.1,4F10.4)
      END
#endif
