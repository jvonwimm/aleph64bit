       SUBROUTINE TPADDP(ISECT,LUN)
C
C-------------------------------------------------------------------
C! Dump TPC pad hit bank (TPAD)
C!
CKEY PRINT TPC PAD / USER
C!
C!     Author:   D. Schlatter    25-08-87
C!     Modified: R. Johnson      12-10-90   Add printout of digits
C!
C!     Input:
C!         ISECT     /I      Sector slot number to dump
C!         LUN       /I      Fortran logical unit number for output
C!
C!------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
C
      PARAMETER (MXDIG=18)
      DIMENSION IDIG(MXDIG)
C
      KTPAD=NLINK('TPAD',ISECT)
      IF(KTPAD.LE.0) GO TO 999
      KTPDI=NLINK('TPDI',ISECT)
C
      WRITE(LUN,'(/A,I2)') ' Dump of bank TPAD for sector ',ISECT
      IND=KTPAD
      IS=0
    1 CONTINUE
        IROWH=IW(IND+1)
        IROW=IBITS(IROWH,0,4)
        NW=IW(IND+2)
        WRITE(LUN,'(1X,I10,A,I2)') NW,' hits in padrow ',IROW
        WRITE(LUN,101)
  101   FORMAT(10X,'  HIT',10X,'T0',8X,'CHA#',6X,'#BUC',6X,'PAD#',
     &             12X,'Digitizations')
        IF (NW.LT.1 .OR. NW.GT.IW(KTPAD)) THEN
          WRITE(LUN,'(A)') ' Data format error.  Exit TPADDP.'
          RETURN
        ENDIF
        DO 2 I=1,NW
          IHIT=IW(IND+2+I)
          IT0=IBITS(IHIT,0,9)
          ICHA=IBITS(IHIT,9,6)
          NBUC=IBITS(IHIT,16,8)
          NPAD=IBITS(IHIT,24,8)
          NDIG=0
          IF (KTPDI.NE.0) THEN
            DO 45 J=1,NBUC
              IS=IS+1
              IWORD=KTPDI+1+(IS-1)/4
              IBIT0=24-8*MOD(IS-1,4)
              IPH=IBITS(IW(IWORD),IBIT0,8)
              IF (NDIG.LT.MXDIG) THEN
                NDIG=NDIG+1
                IDIG(NDIG)=IPH
              ELSE
                GO TO 46
              ENDIF
   45       CONTINUE
   46       CONTINUE
          ENDIF
          WRITE(LUN,'(2X,I5,2X,Z8,4I10,18(1X,I3))')
     &                                 I,IHIT,IT0,ICHA,NBUC,NPAD,
     &                                 (IDIG(K),K=1,NDIG)
    2   CONTINUE
        IND=IND+NW+2
      IF (IND.LT.KTPAD+IW(KTPAD)-1) GO TO 1
      WRITE(LUN,'(A,I2/)') ' End of TPAD dump for sector ',ISECT
C
  999 CONTINUE
      RETURN
      END
#endif
