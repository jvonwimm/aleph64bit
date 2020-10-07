      SUBROUTINE QFCOER(NTK1,NTK2,IREL1,IREL2)
CKEY FILL CAL /INTERNAL
C----------------------------------------------------------------------
C! Set up relations for calorimeter objects
C
C  auxiliary to  QFCOBJ
C                                                    J.Boucrot  23.9.93
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "qcde.h"
C----------------------------------------------------------------------
      IF (KNQLIN + IW(IREL1) .GE. IW(KOQLIN))
     &  CALL QSBANK ('QLIN',KNQLIN+IW(IREL1)+500)
      DO 810 N = 1, IW(IREL1)
C       Take care of double counting in PCRL :
        IF (IW(KOQLIN+IW(IREL1+1)+N) .EQ. NTK2)  GO TO 999
  810 IW(KOQLIN+KNQLIN+N) = IW(KOQLIN+IW(IREL1+1)+N)
      IW(IREL1) = IW(IREL1) + 1
      IW(IREL1+1) = KNQLIN
      KNQLIN = KNQLIN + IW(IREL1)
      IW(KOQLIN+KNQLIN) = NTK2
C
C       now, the reverse relation :
C
      IF (KNQLIN + IW(IREL2) .GE. IW(KOQLIN))
     &    CALL QSBANK ('QLIN',KNQLIN+IW(IREL2)+500)
      DO 820 N=1, IW(IREL2)
  820 IW(KOQLIN+KNQLIN+N) = IW(KOQLIN+IW(IREL2+1)+N)
      IW(IREL2) = IW(IREL2) + 1
      IW(IREL2+1) = KNQLIN
      KNQLIN = KNQLIN + IW(IREL2)
      IW(KOQLIN+KNQLIN) = NTK1
C
 999  END
#endif
