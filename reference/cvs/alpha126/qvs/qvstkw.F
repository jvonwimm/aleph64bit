      SUBROUTINE QVSTKW(ITK,DIRA,WTMA)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Returns track weight matrix in ALEPH system
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  ITK IS ALPHA TRACK INDEX
C  *  DIRA() IS NORMALIZED TRACK DIRECTION IN ALEPH SYSTEM
C      CAN BE OBTAINED BY QVSTKV
C  Output Argument :
C  *  WTMA(,) IS WEIGHT MATRIX IN ALEPH SYSTEM
C
C     THE CHI SQUARE DISTANCE
C     FROM POINT Y() ON THE TRACK TO POINT X() ANYWHERE ELSE IS
C     FOUND BY SUMMING OVER I,J = 1,3
C     (X(I)-Y(I)) * WTMA(I,J) * (X(J)-Y(J))
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION DIRA(3),WTMA(3,3)
      REAL*8 T1,T2
#include "qcde.h"
#include "qmacro.h"
C ------------------------------------------------------------------------------
C
C GET HELIX PARAMETERS
      TL=QFRFTL(ITK)
      ST=1./SQRT(1.+TL**2)
C
C MAKE COVARIANCE NORMAL TO TRACK DIRECTION
      C11=QFRFEM(ITK,4,4)
      C21=QFRFEM(ITK,5,4)*ST
      C22=QFRFEM(ITK,5,5)*ST*ST
C
C CLEAR WEIGHT MATRIX
      CALL UZERO(WTMA,1,9)
C
C DETERMINANT OF COVARIANCE
      T1=C11
      T1=T1*C22
      T2=C21
      T2=T2*C21
      DET=T1-T2
C
      IF (DET .LE. 0.) THEN
C TRACK WEIGHT MATRIX IS NONSENSE
        WRITE (IW(6),19) DET,ITK,KRUN,KEVT
   19   FORMAT (' QVSTKW: DET=',1PE10.3,
     >   ' ITK=',I3,' KRUN=',I6,' KEVT=',I6)
        COR=SQRT(ABS(C11*C22))
        IF (COR .NE. 0.) COR=C21/COR
        WRITE (IW(6),29) C11,C22,COR
   29   FORMAT (' C11=',1PE10.3,' C22=',1PE10.3,' COR=',0PF10.6)
C SET THE CORRELATION TO ZERO
        C21=0.
        WRITE (IW(6),*) ' ## QVSTKW ## : SETTING CORRELATION TO ZERO'
      ENDIF
C
C FILL CORNER OF WEIGHT MATRIX
      WTMA(1,1)=C11
      WTMA(2,1)=C21
      WTMA(1,2)=WTMA(2,1)
      WTMA(2,2)=C22
C
C INVERT THE SUBMATRIX
      CALL RSINV(2,WTMA,3,IFAIL)
C
C CHECK THE RESULT
      IF (IFAIL .NE. 0) THEN
        WRITE (IW(6),39) IFAIL,ITK,KRUN,KEVT
   39   FORMAT (' QVSTKW: IFAIL=',I3,
     >   ' ITK=',I3,' KRUN=',I6,' KEVT=',I6)
        WRITE (IW(6),29) C11,C22,COR
C FILL MATRIX WITH ZEROES
        CALL UZERO(WTMA,1,9)
C AND QUIT
        RETURN
      ENDIF
C
C ROTATE FROM TRACK SYSTEM TO ALEPH SYSTEM
      CALL YVSRMI(DIRA,WTMA,WTMA)
C
      RETURN
      END
#endif
