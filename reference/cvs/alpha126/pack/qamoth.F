      SUBROUTINE QAMOTH (ITKM,NTKD,ITKD)
CKEY QLIN /INTERNAL
C----------------------------------------------------------------------
C! Daughter - mother relation between ITKM and NTKD daughters ITKD.
C  This is trivial if it is a one - to many relation which never will
C  be changed (e.g. : MC "truth"). The complication :
C  Many - to many relation which can be modified at any time.
C  For saving space in bank QLIN (track numbers pointing from mothers
C  to daughters and vice versa), space is reserved essentially in
C  multiples of 8.
C!                                                   H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      DIMENSION ITKD(100)
C
      NMO(I) = KOQVEC+I*KCQVEC+JQVENO
      LMO(I) = KOQVEC+I*KCQVEC+JQVEOL
      NDA(I) = KOQVEC+I*KCQVEC+JQVEND
      LDA(I) = KOQVEC+I*KCQVEC+JQVEDL
      MUL8(N) = ((N + 7) / 8) * 8
C
      DO 50 ND=1,NTKD
      ITKN = ITKD(ND)
C
      NM = IW(NMO(ITKN))
      IF (NM .EQ. 0)  THEN
C
C         First mother of ITKN :
C
        IF (KNQLIN .GE. IW(KOQLIN))
     &    CALL QSBANK ('QLIN', KNQLIN + 500)
        IW(NMO(ITKN)) = 1
        IW(LMO(ITKN)) = KNQLIN
        KNQLIN = KNQLIN + 1
        IW(KOQLIN+KNQLIN) = ITKM
C
C         Other mother(s) of ITKM exist already :
C
      ELSE
        NRES = MUL8 (NM + 1)
C           new area in QLIN ? Always multiples of 8.
        IF (NM .LT. 8 .OR. NRES .GT. MUL8 (NM))  THEN
C           extend QLIN if necessary
          IF (KNQLIN + NRES .GE. IW(KOQLIN))
     &      CALL QSBANK ('QLIN', KNQLIN + NRES + 500)
C           copy old mother track numbers
          DO 40 N=1,NM
   40     IW(KOQLIN+KNQLIN+N) = IW(KOQLIN+IW(LMO(ITKN))+N)
          IW(LMO(ITKN)) = KNQLIN
          KNQLIN = KNQLIN + NRES
        ENDIF
C           fill pointer daughter --> QLIN
        IW(NMO(ITKN)) = IW(NMO(ITKN)) + 1
        IW(KOQLIN+IW(LMO(ITKN))+IW(NMO(ITKN))) = ITKM
      ENDIF
   50 CONTINUE
C
      END
#endif
