      FUNCTION QKBLOK(IRUN,BG,Q)
CKEY DEDX /INTERNAL
C----------------------------------------------------------------------
C! Bethe-Bloch parameterization of TPC dE/dx which always uses the
C! parameterization for data (run 6001) regardless of current run
C! number (this is to be used for MC simulation).
C!
C!   Created by Robert P. Johnson     22-AUG-1988
C!
C!   Inputs    : IRUN      /I      Run number for calibration constants
C!               BG        /R      beta*gamma
C!               Q         /R      particle charge
C!   Outputs   : QKBLOK    /R      dE/dx with minion=1.0
C!                                 Set to zero if Q=0 or if the
C!                                 calibration cannot be found.
C!
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "tc4xjj.h"
      LOGICAL FIRST
      DATA LASRN/-1/,FIRST/.TRUE./,KUPRNT/6/
#include "bmacro.h"
C----------------------------------------------------------------------
C++   Get new calibration constants if the run number has changed
C
      IF (IRUN.NE.LASRN) THEN
        LASRN=IRUN
C
C++     Read the calibration bank from the database
C
        NNR=NDANR(JUNIDB(0),'TC4X','LE',IRUN)
        IF (NNR.EQ.0) THEN
          CALL QMTERM('_QDEDXM_ Cannot find TC4X on database.')
        ENDIF
        KTC4X=MDARD(IW,JUNIDB(0),'TC4X',NNR)
        IF (KTC4X.EQ.0) THEN
          CALL QMTERM('_QDEDXM_ Insufficient space for bank TC4X.')
        ENDIF
C
        IF (NNR.LT.2001 .AND. FIRST) THEN
          WRITE(KUPRNT,98) IRUN,IRUN,NNR,NNR
   98     FORMAT(//' QKBLOK:  error in accessing dE/dx constants from',/
     &    ' the database.  QDEDXM specified that constants for run ',
     &    I6,/' be used in the parameterization of the dE/dx curve',/
     &    ' (bank TC4X), but the largest NR in the database less than',/
     &    ' or equal to ',I6,' is ',I4,', which is a Monte Carlo run.',/
     &    ' QDEDXM must have the constants fit to the DATA that are',/
     &    ' to be simulated.  Maybe you need to explicitely specify',/
     &    ' that the 89/90 database be selected, using the FDBA card.',/
     &    ' I will continue now, using the constants from NR=',I4,/)
          FIRST=.FALSE.
        ENDIF
C
C++     Save the appropriate calibration constants
C
        IB=1
        XI=RTABL(KTC4X,IB,JTC4IP)
        P= RTABL(KTC4X,IB,JTC4IP+1)
        RK=RTABL(KTC4X,IB,JTC4IP+2)
        X0=RTABL(KTC4X,IB,JTC4IP+3)
        X1=RTABL(KTC4X,IB,JTC4IP+4)
        A3=RTABL(KTC4X,IB,JTC4IP+5)*1.0E-3
        A4=RTABL(KTC4X,IB,JTC4IP+6)*1.0E-6
        A5=RTABL(KTC4X,IB,JTC4IP+7)*1.0E-7
        A6=RTABL(KTC4X,IB,JTC4IP+8)*1.0E-8
        A7=RTABL(KTC4X,IB,JTC4IP+9)*1.0E-9
C
C++     Drop the bank which was read in
C
        KTC4X=NDROP('TC4X',NNR)
      ENDIF
C
C++   Neutral tracks don't have any dE/dx
C
      IF (Q.EQ.0.) THEN
        QKBLOK=0.
        GO TO 999
      ENDIF
C
      Q2=Q**2
      X=ALOG(BG)
      BETA = BG/(SQRT(1.+BG**2))
      BP=BETA**P
      T= X0-X1
      SUMD= (((((7.*A7*T+6.*A6)*T+5.*A5)*T+4.*A4)*T+3.*A3)*T)*T
      A2= -(1.+0.5*SUMD)/T
      SUM= ((((((A7*T+A6)*T+A5)*T+A4)*T+A3)*T+A2)*T)*T
      XA= X0 + 0.5*SUM
      IF (X.LT.X0) THEN
        DEL=0.
      ELSEIF (X.LT.X1) THEN
        T=X-X1
        SUM= ((((((A7*T+A6)*T+A5)*T+A4)*T+A3)*T+A2)*T)*T
        DEL= 2.*(X-XA) + SUM
      ELSE
        DEL= 2.*(X-XA)
      ENDIF
      QKBLOK= (XI*Q2/BP)*(RK+2.*X-BP-DEL)
C
  999 RETURN
      END
#endif
