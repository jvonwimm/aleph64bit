      SUBROUTINE QADAU (ITKM,NTKD,ITKD)
CKEY QLIN /INTERNAL
C----------------------------------------------------------------------
C! Mother - daughter relation between ITKM and NTKD daughters ITKD.
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
      NDA(I) = KOQVEC+I*KCQVEC+JQVEND
      LDA(I) = KOQVEC+I*KCQVEC+JQVEDL
      MUL8(N) = ((N + 7) / 8) * 8
C
      ND = IW(NDA(ITKM))
      IF (ND .EQ. 0)  THEN
C
C         First daughter(s) of ITKM :
C
        IF (NTKD .LE. 8)  THEN
C           reserve 1,2,3,4,5,6,7,8 locations in QLIN
          NRES = NTKD
        ELSE
C           reserve 16,24,32,40,... locations in QLIN
          NRES = MUL8 (NTKD)
        ENDIF
C           extend QLIN if necessary
        IF (KNQLIN + NRES .GE. IW(KOQLIN))
     &    CALL QSBANK ('QLIN', KNQLIN + NRES + 500)
C           fill track numbers into QLIN
        DO 10 N=1,NTKD
   10   IW(KOQLIN+KNQLIN+N) = ITKD(N)
C           fill pointers mother --> QLIN
        IW(NDA(ITKM)) = NTKD
        IW(LDA(ITKM)) = KNQLIN
C           reserve space in QLIN
        KNQLIN = KNQLIN + NRES
      ELSE
C
C         Other daughter(s) of ITKM exist already :
C
        NRES = MUL8 (ND + NTKD)
C           new area in QLIN ? Always multiples of 8.
        IF (ND .LT. 8 .OR. NRES .GT. MUL8 (ND))  THEN
C           extend QLIN if necessary
          IF (KNQLIN + NRES .GE. IW(KOQLIN))
     &      CALL QSBANK ('QLIN', KNQLIN + NRES + 500)
C           copy old daughter track numbers
          DO 20 N=1,ND
   20     IW(KOQLIN+KNQLIN+N) = IW(KOQLIN+IW(LDA(ITKM))+N)
C           fill pointer mother --> QLIN
          IW(LDA(ITKM)) = KNQLIN
C           reserve space in QLIN
          KNQLIN = KNQLIN + NRES
        ENDIF
C           fill new daughter track numbers into QLIN
        DO 30 N=1,NTKD
   30   IW(KOQLIN+IW(LDA(ITKM))+IW(NDA(ITKM))+N) = ITKD(N)
        IW(NDA(ITKM)) = IW(NDA(ITKM)) + NTKD
      ENDIF
C
      END
#endif
