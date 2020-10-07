      SUBROUTINE PI0DEB
CKEY QPI0DO / USER
C-----------------------------------------------------------------------
C! Debug printout of all Pi0s found by QPI0DO
C    Author   :  J-P Lees              15-Oct-1992
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "gampi0.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
      WRITE (IW(6),1000)  NTPI0
 1000 FORMAT(1X,' ### QPI0DO ### ', I3, ' Pi0 found')
      DO IPI0 = 1, NTPI0
        WRITE (IW(6),1001)IPI0,(PI0MOM(J,IPI0),J=1,4),ITYPI0(IPI0),
     +               IPI0GAM(1,IPI0),IPI0GAM(2,IPI0)
        I1 = IPI0GAM(1,IPI0)
        I2 = IPI0GAM(2,IPI0)
        ESUM = QE(I1)+QE(I2)
        AMASS= SQRT(ESUM**2-(QX(I1)+QX(I2))**2-
     +                      (QY(I1)+QY(I2))**2-
     +                      (QZ(I1)+QZ(I2))**2)
        WRITE (IW(6),1002)ESUM, AMASS, CHIPI0(IPI0)
      ENDDO
 1001 FORMAT(1X,'pi0#',i3,' Px,Py,Pz,E :',4f6.1,' ty:',i1,
     +     ' g1:',i3,' g2:',i3 )
 1002 FORMAT(1X,'        E(unfitted):',f6.1,' Mass:', f6.4, ' chi2:',
     +       f8.3)
 999  RETURN
      END
#endif
