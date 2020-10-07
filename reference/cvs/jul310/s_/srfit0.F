      SUBROUTINE SRFIT0(N,THRAW,PHRAW,SIGMA,AX,AY,CHISQ,COVXY,IER)
C----------------------------------------------------------------------
C! Perform straight line fit
C!
C!    Author:       H. Meinhard  21-Mar-1987
C!    Modified:     H. Meinhard  30-Mar-1988
C!    Modified:     D.Smith      991027 Double precision fixes for Linux
C!
C!    Input:        - N           /I    number of coord. ([1,9])
C!                  - THRAWi      /R    tan theta of coord.
C!                  - PHRAWi      /R    azimuth angle of coord. centre
C!                  - SIGMAi      /R    error in THRAWi
C!    Output:       - AX          /R    x/z of fitted line
C!                  - AY          /R    y/z of fitted line
C!                  - CHISQ       /R    chisquare of fit
C!                  - COVXYij     /R    covariance matrix of AX, AY
C!                  - THRAWi      /R    difference between measured
C!                                      and fitted THRAW, divided by
C!                                      SIGMAi
C!                  - IER         /I    error flag: .ne. 0 if error
C!
C!    Description
C!    ===========
C!    Fit a straight line through the origin of the coordinate system
C!    to measured wire paralleles given by the tangent of their polar
C!    angle and their azimuth angle, both with respect to the center
C!    point of the wire parallele.
C!    Internally, calculations are performed in Double Precision. If
C!    the determinant of the coefficient matrix is less than 1000.,
C!    error flag is set.
C?
C!======================================================================
#ifndef DOC
#include "sgeomc.h"
      DOUBLE PRECISION USQUA,VSQUA,YSQUA,UTIMV,UTIMY,VTIMY,DETA,
     +                 DAX,DAY,SIGSQ,SINUS(MLASG),COSIN(MLASG),D2
      REAL THRAW(*),PHRAW(*),SIGMA(*),COVXY(2,*)
C----------------------------------------------------------------------
      IF (N.GT.MLAYSG)                                      GOTO 901
      D2 = DBLE(2.)
C
C calculate vector products
      USQUA = 0D0
      VSQUA = 0D0
      YSQUA = 0D0
      UTIMV = 0D0
      UTIMY = 0D0
      VTIMY = 0D0
      DO 300 I = 1, N
        IF (SIGMA(I).LT.1.E-8)                             GOTO 902
        SINUS(I) = DSIN(DBLE(PHRAW(I)))
        COSIN(I) = DCOS(DBLE(PHRAW(I)))
        SIGSQ = DBLE(SIGMA(I)**2)
        USQUA = USQUA+SINUS(I)**2/SIGSQ
        VSQUA = VSQUA+COSIN(I)**2/SIGSQ
        UTIMV = UTIMV+SINUS(I)*COSIN(I)/SIGSQ
        UTIMY = UTIMY+DBLE(THRAW(I))*SINUS(I)/SIGSQ
        VTIMY = VTIMY+DBLE(THRAW(I))*COSIN(I)/SIGSQ
  300 CONTINUE
C
C calculate determinant of coefficient matrix
      DETA = USQUA*VSQUA-UTIMV**2
      IF (DETA .LT. 1000D0)                                  GOTO 903
C
C calculate AX and AY, covariance matrix
      DAX = (USQUA*VTIMY-UTIMV*UTIMY)/DETA
      DAY = (VSQUA*UTIMY-UTIMV*VTIMY)/DETA
      AX = REAL(DAX)
      AY = REAL(DAY)
      COVXY(1,1) = REAL(USQUA/DETA)
      COVXY(1,2) = REAL(-UTIMV/DETA)
      COVXY(2,1) = COVXY(1,2)
      COVXY(2,2) = REAL(VSQUA/DETA)
C
C calculate chisquare, error vector
      CHISQ = 0.
      DO 310 I = 1, N
  310 CHISQ = CHISQ + ((THRAW(I) - AX*REAL(COSIN(I)) -
     +                  AY*REAL(SINUS(I))) / SIGMA(I)) ** 2
      DO 320 I = 1, N
  320 THRAW(I) = (THRAW(I)-REAL(COSIN(I)*DAX-SINUS(I)*DAY)) / SIGMA(I)
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      GOTO 999
  903 IER = 3
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
