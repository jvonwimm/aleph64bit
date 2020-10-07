      SUBROUTINE YINVMX(A,O,L,IFAIL)
C----------------------------------------------------------*
C!    invert upper triangle of symmetric matrix stored row wise
CKEY YTOP
C!    Author :     G. Lutz     /11/90
C!
C!
C!    Description
C!    ===========
C!    invert upper triangle of symmetric matrix stored row wise
C!
C!    A ...... matrix input
C!    O ...... inverted matrix output
C!    L ...... matrix dimension
C!
C!
C----------------------------------------------------------*
#ifndef DOC
      SAVE
#include "bcs.h"
C
      DIMENSION A(*),O(*),B(900)
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
      IF(L.GT.30) WRITE(LOUT,10) L
   10 FORMAT (' ======= YINVMX CALLED WITH TOO HIGH DIMENSION =',I5)
C
      IDX=0
      DO 100 I=1,L
        DO 100 J=1,I
          IDX=IDX+1
          I1=(I-1)*L+J
          I2=(J-1)*L+I
          B(I1)=A(IDX)
          B(I2)=A(IDX)
  100 CONTINUE
      CALL RSINV(L,B,L,IFAIL)
      IDX=0
      DO 200 I=1,L
        DO 200 J=1,I
          IDX=IDX+1
          I1=(I-1)*L+J
          O(IDX)=B(I1)
  200 CONTINUE
      RETURN
      END
#endif
