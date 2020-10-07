      SUBROUTINE QDEDX(ITK,N,RMASS,QH,RI,NS,TL,RIEXP,SIGMA,IER)
CKEY DEDX /USER
C----------------------------------------------------------------------
C! dE/dx analysis in ALPHA
C! ALPHA interface to the ALEPHLIB routine TIDHYP
C
C  Author:  R. Johnson   27-07-89
C  Modified :  G. Graefe    25-04-94 some changes for NanoDst input
C
C     Input:   ITK      /I       ALPHA track number
C              N        /I       Number of hypothesis to try
C              RMASS(n) /R       Mass hypotheses
C              QH(n)     /R       Charge hypotheses (sign doesn't matter
C     Output:  RI       /R       Measured ionization (1.O=minion, Q=1)
C              NS       /R       Number of useful wire samples on track
C              TL       /T       Useful length of the track (cm)
C              RIEXP(n) /R       Expected ionization for the given
C                                mass hypothesis (1.0=minion, Q=1)
C              SIGMA(n) /R       Sigma of dE/dx measurement error,
C                                including the momentum error.
C                                Note that one can calculate a
C                                chi-squared with 1 d.o.f. as:
C                                chi2 = ((RI-RIEXP)/SIGMA)**2
C              IER      /I       Error return= 0 for success
C                                1= can't find track bank or not
C                                   a reconstructed charged track
C                                2= can't find dE/dx bank
C                                3= track has no dE/dx information
C                                4= can't find calibration banks
C                                   TC1X, TC2X, and/or TC3X
C                                5= cannot find RUNH or EVEH bank
C                                   from which to get the run number
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      DIMENSION RMASS(N),QH(N),RIEXP(N),SIGMA(N)
#include "qmacro.h"
C----------------------------------------------------------------------
      IF (XNANO) THEN
        CALL NADDX(ITK,N,RMASS,QH,RI,NS,TL,RIEXP,SIGMA,IER)
        GOTO 999
      ENDIF
      IF (KCLASS(ITK).NE.KRECO) THEN
        IER=1
        GOTO 999
      ENDIF
      IF (QCH(ITK).EQ.0.) THEN
        IER=1
        GOTO 999
      ENDIF
      IT1=KCHT(ITK)
      CALL TIDHYP(KTN(IT1),QMFLD,N,RMASS,QH,RI,NS,TL,RIEXP,SIGMA,IER)
999   END
#endif
