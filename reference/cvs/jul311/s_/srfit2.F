      SUBROUTINE SRFIT2(ISACP,CHI2,XFIT,COVX,IER)
C----------------------------------------------------------------------
C! Perform double arm straight line fit
C!
C!    Author:     H. Meinhard       02-Feb-1990
C!    Modified:   H. Meinhard       28-Mar-1990  (4)
C!    Modified:   D.Smith     991027 Double precision fixes for Linux
C!
C!    Input:      - ISACPi    /I    pointer to elements of bank SACO
C!                                  which will be used in fit; must
C!                                  end with 0
C!    Output:     - CHI2      /F    chisquare of fit
C!                - XFITi     /F    vector of fitted parameters:
C!                                  (theta,phi,x0,y0)
C!                - COVXij    /F    matrix of covariance elements
C!                - IER       /I    error code: 1 = missing input banks
C!                                              2 = singularity
C!
C!    Description
C!    ===========
C!    Fit a straight line to measurements of SATR coordinates on both
C!    sides of the interaction point. Coordinates are taken from bank
C!    SACO.
C!    For the fit formulas, see calculation in Logbook 4, pg. 82 ff.
C?
C!======================================================================
#ifndef DOC
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "sacojj.h"
#include "srecpc.h"
C NR is the number of fitted parameters, NN the maximum number of mea-
C surements
      PARAMETER (NR=4,NN=18)
      INTEGER ISACP(*)
      REAL XFIT(*),COVX(NR,*)
      DOUBLE PRECISION A(NN,NR),B(NN),C(NN),GB(NN),ATGB(NR,NN),
     +  CX(NR,NR),BEPS(NN),XI(NR),WORK(NN),T(NR,NR),DEN1,DEN2
      REAL X0(NR),BF(3),BR(3),BW(3)
      EXTERNAL DVMPY,PROXIM
#include "bmacro.h"
C----------------------------------------------------------------------
C link to bank SACO
      JSACO = IW(NASACO)
      IF (JSACO .EQ. 0)                                     GOTO 901
C set the initial values for the parameters: impact points: zero;
C declinations: calculate mean value for coordinates
      N = 0
      C1 = 0.
      C2 = 0.
      DO 300 IN = 1, NN
        IP = ISACP(IN)
        IF (IP .NE. 0) THEN
          X2 = RTABL(JSACO,IP,JSACBF) +
     +         RTABL(JSACO,IP,JSACRA) * RTABL(JSACO,IP,JSACBR)
          Y2 = RTABL(JSACO,IP,JSACBF+1) +
     +         RTABL(JSACO,IP,JSACRA) * RTABL(JSACO,IP,JSACBR+1)
          Z2 = RTABL(JSACO,IP,JSACBF+2) +
     +         RTABL(JSACO,IP,JSACRA) * RTABL(JSACO,IP,JSACBR+2)
          N = N + 1
          C1 = C1 + X2 / Z2
          C2 = C2 + Y2 / Z2
        ENDIF
  300 CONTINUE
      IF (N .NE. 0) THEN
        C1 = C1 / REAL(N)
        C2 = C2 / REAL(N)
      ENDIF
      X0(1) = C1
      X0(2) = C2
      X0(3) = 0.
      X0(4) = 0.
C fill matrices A = (dfi / dxk), B = (dfi / dyk),
C and vector c = fi (x0, y0)
C Since B is diagonal, we store it as a vector
C N is the number of coordinates in fit
      N = 0
      DO 310 IN = 1, NN
        IP = ISACP(IN)
        IF (IP .EQ. 0)                                      GOTO 320
        N = N + 1
        KSACO = KROW(JSACO,IP)
        RA = RW(KSACO+JSACRA)
        CALL UCOPY(RW(KSACO+JSACBF),BF,3)
        CALL UCOPY(RW(KSACO+JSACBR),BR,3)
        CALL UCOPY(RW(KSACO+JSACBW),BW,3)
        A(N,1) = DBLE( -BW(3) * (BF(2) + RA*BR(2) - X0(4)) +
     +    BW(2) * (BF(3) + RA*BR(3)) )
        A(N,2) = DBLE(  BW(3) * (BF(1) + RA*BR(1) - X0(3)) -
     +    BW(1) * (BF(3) + RA*BR(3)) )
        A(N,3) = DBLE( -X0(2) * BW(3) + BW(2) )
        A(N,4) = DBLE(  X0(1) * BW(3) - BW(1) )
        B(N) = DBLE(
     +    (X0(2) * BW(3) - BW(2)) * BR(1) -
     +    (X0(1) * BW(3) - BW(1)) * BR(2) +
     +    (X0(1) * BW(2) - X0(2) * BW(1)) * BR(3) )
        C(N) = DBLE(
     +    (X0(2) * BW(3) - BW(2)) * (BF(1) + RA*BR(1) - X0(3)) -
     +    (X0(1) * BW(3) - BW(1)) * (BF(2) + RA*BR(2) - X0(4)) +
     +    (X0(1) * BW(2) - X0(2) * BW(1)) * (BF(3) + RA*BR(3)) )
        GB(N) = 1D0 / (DBLE(SIG2SR) * B(N)) ** 2
  310 CONTINUE
  320 CONTINUE
C calculate ATGB
      CALL DRSCL(N,NR,GB(1),GB(2),A(1,1),A(1,2),A(2,1),
     +  ATGB(1,1),ATGB(2,1),ATGB(1,2))
C calculate CX
      CALL DMMLT(NR,N,NR,ATGB(1,1),ATGB(1,2),ATGB(2,1),
     +  A(1,1),A(1,2),A(2,1),CX(1,1),CX(1,2),CX(2,1),WORK)
      CALL DINV(NR,CX,NR,WORK,IER)
      IF (IER .EQ. -1)                                      GOTO 902
C calculate XI
      CALL DMMPY(NR,N,ATGB(1,1),ATGB(1,2),ATGB(2,1),C(1),C(2),
     +  XI(1),XI(2))
      CALL DMMPY(NR,NR,CX(1,1),CX(1,2),CX(2,1),XI(1),XI(2),
     +  WORK(1),WORK(2))
      CALL DVSCL(NR,-1.D0,WORK(1),WORK(2),XI(1),XI(2))
C calculate (B * Epsilon) / (SIGMA * B)
      CALL DVCPY(N,C(1),C(2),WORK(1),WORK(2))
      CALL DMMPA(N,NR,A(1,1),A(1,2),A(2,1),XI(1),XI(2),WORK(1),WORK(2))
      CALL DVMUL(N,GB(1),GB(2),WORK(1),WORK(2),BEPS(1),BEPS(2))
      CALL DVMUL(N,B(1),B(2),BEPS(1),BEPS(2),WORK(1),WORK(2))
      CALL DVSCL(N,DBLE(SIG2SR),WORK(1),WORK(2),BEPS(1),BEPS(2))
C calculate chisquare
      CHI2 = REAL(DVMPY(N,BEPS(1),BEPS(2),BEPS(1),BEPS(2)))
C transform c1 and c2 into theta and phi
      C1 = X0(1) + REAL(XI(1))
      C2 = X0(2) + REAL(XI(2))
      CC = C1**2 + C2**2
      THETA = ATAN(SQRT(CC))
      PHI = PROXIM(ATAN2(C2,C1),PI)
C fill vector with fitted parameters
      XFIT(1) = THETA
      XFIT(2) = PHI
      XFIT(3) = X0(3) + REAL(XI(3))
      XFIT(4) = X0(4) + REAL(XI(4))
C define transformation matrix
      DO 326 I = 1, NR
        DO 325 J = 1, NR
          T(J,I) = 0D0
  325   CONTINUE
  326 CONTINUE
      DEN1 = 1D0 / DBLE( (1.+CC)*SQRT(CC) )
      DEN2 = 1D0 / DBLE( CC )
      T(1,1) = DBLE(C1) * DEN1
      T(1,2) = DBLE(C2) * DEN1
      T(2,1) = -DBLE(C2) * DEN2
      T(2,2) = DBLE(C1) * DEN2
      T(3,3) = 1D0
      T(4,4) = 1D0
C calculate transformed covariance matrix
      CALL DMMLT(NR,NR,NR,CX(1,1),CX(1,2),CX(2,1),T(1,1),T(2,1),T(1,2),
     +  CX(1,1),CX(1,2),CX(2,1),WORK)
      CALL DMMLT(NR,NR,NR,T(1,1),T(1,2),T(2,1),CX(1,1),CX(1,2),CX(2,1),
     +  CX(1,1),CX(1,2),CX(2,1),WORK)
      DO 340 J = 1, NR
        DO 330 I = 1, NR
          COVX(I,J) = REAL(CX(I,J))
  330   CONTINUE
  340 CONTINUE
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRFIT2',IER,'Matrix to be inverted is singular')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
#endif
