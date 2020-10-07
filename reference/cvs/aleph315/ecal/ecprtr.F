      SUBROUTINE ECPRTR
C-------------------------------------------------------------------
C      O.CALLOT   29-JAN-86
C
C! Print trigger banks ETTR and EWTR
C
C-------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      DIMENSION IPVAL(12),LIMTH(12)
      CHARACTER*10 NAMOD(3)
C
#include "bmacro.h"
      DATA LIMTH / 8,24,46,50,88,114,140,178,182,204,220,228/
      DATA NAMOD / 'End Cap A' , 'Barrel' , 'End Cap B' /
C ----------------------------------------------------------
      LOUT = IW(6)
C
      JETTR = NLINK ('ETTR',0)
      IF( JETTR .EQ. 0 ) GOTO 500
      KETTR = JETTR + LMHLEN
      WRITE(LOUT,1000) LIMTH
      DO 100 IPH = 1,24
        IND = KETTR + 1
        DO 110 J=1,12
          IPVAL(J) = NINT( .001 * (IW(IND) + IW(IND+1) + IW(IND+2)) )
          IND = IND + 72
  110   CONTINUE
        WRITE(LOUT,1010) IPH,IPVAL
        KETTR = KETTR + 3
  100 CONTINUE
C
  500 CONTINUE
      JEWTR = NLINK ('EWTR',0)
      IF( JEWTR .EQ. 0 ) GOTO 900
      KEWTR = JEWTR + LMHLEN
      WRITE(LOUT,1100)
      DO 510 JM=1,3
        DO 520 IM = 1,12
          IPVAL(IM) = NINT( .001 * (IW(KEWTR+1) + IW(KEWTR+2)) )
          KEWTR = KEWTR + 2
  520   CONTINUE
        WRITE(LOUT,1110) NAMOD(JM),IPVAL
  510 CONTINUE
  900 CONTINUE
      RETURN
 1000 FORMAT (/1X,'+++ECPRTR+++ ETTR tower trigger print out in MEV'//
     + ' Theta limits   0',12(4X,I3)/)
 1010 FORMAT(' Phi bin nb ',I2,1X,12I7)
 1100 FORMAT (/1X,'+++ECPRTR+++ EWTR wire trigger print out in MEV'//)
 1110 FORMAT(2X,A10,2X,12I7)
      END
#endif
