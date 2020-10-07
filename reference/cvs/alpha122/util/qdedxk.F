      SUBROUTINE QDEDXK(IRUN,P,Q,RMASS,DX,NSMP,DEDX,EDEDX,SIGMA)
CKEY DEDX MONTE /INTERNAL
C----------------------------------------------------------------------
C! Fast, simple dE/dx simulation of a single track.
C
C  R. Johnson   5-02-90
C  Input:    IRUN       /I       Run number for calibration constants
C            P          /R       Track momentum
C            Q          /R       Particle charge
C            RMASS      /R       Particle mass
C            DX         /R       Average sample length
C            NSMP       /I       Number of dE/dx samples
C  Output:   DEDX       /R       Simulated dE/dx
C            EDEDX      /R       Expected mean dE/dx
C            SIGMA      /R       dE/dx resolution
C----------------------------------------------------------------------
#ifndef DOC
      SAVE FIRST,LASRN,KUPRNT
#include "bcs.h"
#include "tc4xjj.h"
C
      LOGICAL FIRST
      DATA LASRN/-1/,FIRST/.TRUE./,KUPRNT/6/
C
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
   98     FORMAT(//' QDEDXK:  error in accessing dE/dx constants from',/
     &    ' the database.  QDEDXM specified that constants for run ',
     &    I6,/' be used in the parameterization of the resolution',/
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
        PCUT= RTABL(KTC4X,IB,JTC4RP)
        A1= RTABL(KTC4X,IB,JTC4RP+3)
        B1= RTABL(KTC4X,IB,JTC4RP+4)
        A2= RTABL(KTC4X,IB,JTC4RP+1)
        B2= RTABL(KTC4X,IB,JTC4RP+2)
        FLG=RTABL(KTC4X,IB,JTC4RP+6)
C
C++     Drop the bank which was read in
C
        KTC4X=NDROP('TC4X',NNR)
      ENDIF
C
      IF (P.GT.PCUT) THEN
        AA=A1
        BB=B1
      ELSE
        AA=A2
        BB=B2
      ENDIF
C
      BG=P/RMASS
C
      EDEDX=QKBLOK(IRUN,BG,Q)
      IF (FLG.EQ.0.) THEN
        ARG=(AA/FLOAT(NSMP))+BB
        SIGMA= SQRT(ARG)
      ELSE
        SIGMA=PCUT*(DX**B2)*((FLOAT(NSMP))**A2)
      ENDIF
C
      CALL RANNOR(RNDNM,YDUM)
      DEDX= EDEDX*(1. + SIGMA*RNDNM)
C
      RETURN
      END
#endif
