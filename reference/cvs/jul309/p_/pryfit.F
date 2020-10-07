      SUBROUTINE PRYFIT
C----------------------------------------------------------
C!   -Print out contents of vertex fit banks
C!    Author: M. Mermikides 9 Sept 1987
C ----------------------------------------------------------------
#ifndef DOC
#ifndef DOC
#include "bcs.h"
#include "yynamc.h"
#include "yvxljj.h"
#include "ytrljj.h"
#include "bmacro.h"
      KYVXL = IW(NAYVXL)
      IF (KYVXL.EQ.0) GO TO 999
      LUNIT = IW(6)
      WRITE(LUNIT,1000)
 1000 FORMAT(' ++PRYFIT++ Contents of Vertex fit bank YVXL'/,
     & 3X,'VxTyp',3X,'X',8X,'Y',8X,'Z',7X,'ErrMat (xx,xy,yy,xz,yz,zz)',
     &  33X,'Chi2',5X,'Ndf',2X,'JT',3X,'NT'/)
      DO 10 I=1,IW(KYVXL+LMHROW)
         WRITE (LUNIT,1001) IW(KROW(KYVXL,I)+JYVXVT),
     &    (RW(KROW(KYVXL,I)+JYVXXP+M),M=0,2),
     &    (RW(KROW(KYVXL,I)+JYVXEM+M),M=0,5),
     &    RW(KROW(KYVXL,I)+JYVXCH),IW(KROW(KYVXL,I)+JYVXDF),
     &    (IW(KROW(KYVXL,I)+JYVXOF+M),M=0,1)
   10 CONTINUE
 1001 FORMAT(2X,I3,3F9.3,7E10.3,3I5)
C
      KYTRL = IW(NAYTRL)
      WRITE(LUNIT,1100)
 1100 FORMAT(' ++PRYFIT++ Contents of beam const. fit bank YTRL'/,
     & 5X,'Q/rho',4X,'tandip',3X,'phi0',5X,'d0',7X,'z0',7X,
     &'ErrMat (11 12 22 13 23 33)',39X,'NV',3X,'NT'/)
      DO 11 I=1,IW(KYTRL+LMHROW)
         WRITE (LUNIT,1011) (RW(KROW(KYTRL,I)+M),M=1,11),
     &       IW(KROW(KYTRL,I)+JYTRVN),IW(KROW(KYTRL,I)+JYTRTN)
   11 CONTINUE
 1011 FORMAT(1X,5F9.3,6E11.3,2I5)
C
  999 RETURN
      END
#endif
#endif
