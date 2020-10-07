      SUBROUTINE NADDX(ITK,N,RMASS,QH,RI,NS,TL,RIEXP,SIGMA,IER)
CKEY ALPHA DEDX FROM NANO /INTERNAL
C----------------------------------------------------------------------
C!  - dE/dx analysis from the NANO
C!
C!   Author   :- Yves A. Maumary       23-OCT-1992
C!   Modified version of R. Johnson's QDEDX routine
C!   Modified :- Gerrit Graefe         25-APR-1994 get dE/dx for ALPHA
C!                                                 if input comes from a
C!                                                 NanoDst
C!
C!    Input:   ITK      /I       SANDY track number
C!             N        /I       Number of hypothesis to try
C!             RMASS(n) /R       Mass hypotheses
C!             QH(n)    /R       Charge hypotheses (sign doesn't matter)
C!    Output:  RI       /R       Measured ionization (1.O=minion, Q=1)
C!             NS       /I       Number of useful wire samples on track
C!             TL       /R       Useful length of the track (cm)
C!             RIEXP(n) /R       Expected ionization for the given
C!                               mass hypothesis (1.0=minion, Q=1)
C!             SIGMA(n) /R       Sigma of dE/dx measurement error,
C!                               including the momentum error.
C!                               Note that one can calculate a
C!                               chi-squared with 1 d.o.f. as:
C!                               chi2 = ((RI-RIEXP)/SIGMA)**2
C!             IER      /I       Error return= 0 for success
C!                               1= can't find track bank or not
C!                                  a reconstructed charged track
C!                               2= can't find dE/dx bank
C!                               3= track has no dE/dx information
C!                               4= can't find calibration banks
C!                                  TC1X, TC2X, and/or TC3X
C!                               5= cannot find RUNH or EVEH bank
C!                                  from which to get the run number
C!
C!   Libraries required: ALEPHLIB
C!
C!   Description
C!   ===========
C!   Get the TC3X calibration bank for the current run, apply it on the
C!   measured ionisation from QRIMES (TC3X is NOT applied at the nanoDST
C!   production step), and finally call TXDEDX for the expected
C!   ionisation and error for the various mass hypotheses.
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
      SAVE FIRST,NTC3X,TWRSP,IROLD,NWAR
      EXTERNAL JUNIDB
#include "qdecl.h"
#include "qcde.h"
      INTEGER ITK,NS,IER,N
      REAL    RMASS(N),QH(N),RI,TL,RIEXP(N),SIGMA(N)
C
#include "tc3xjj.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "ndtkjj.h"
#include "nddejj.h"
      INTEGER KRNDTK,KRNDDE,IROWT,IROWD,INDNTK,INDNDE,I,J,NLINK
      INTEGER MASK8,MASK9,MASK12,MASK20,MASK23
      DATA    MASK8,MASK9,MASK12,MASK20,MASK23/255,511,4095,1048575,
     &                                         8388607/
C
      INTEGER ALGTDB,JUNIDB,NAMIND,GTSTUP,NTC3X,KTC3X,IRET,IERR
      REAL    P,SGP,RSIG,SMP,SMPL,BG,EBG
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER IRUN,IEVT,IROLD,ITP
      DATA IROLD/0/
      CHARACTER DET*2,LIST*4
      PARAMETER(DET='TP',LIST='TC3X')
      CHARACTER*80 MSG
      INTEGER NWAR
      DATA NWAR/0/
C
C++   Spacing between sense wires in the TPC in cm
C
      REAL    TWRSP
      DATA TWRSP/0.4/
C
#include "qmacro.h"
C-----------------------------------------------------------------------
      RI=0.0
      TL=0.0
      NS=0
      CALL VZERO(RIEXP,N)
      CALL VZERO(SIGMA,N)
      IF(QCH(ITK).EQ.0.)THEN
        IER=1
        RETURN
      ENDIF
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        NTC3X=NAMIND('TC3X')
      ENDIF
C
C++ Get the TC3X calibration bank
C++ Get banks from DB depending on run and setup code
C
      CALL ABRUEV (IRUN,IEVT)
      IRET = 0
      IF (IRUN.NE.IROLD) THEN
        IROLD = IRUN
        IF (IRUN.LE.2000) THEN
           ITP = GTSTUP (DET,IRUN)
        ELSE
           ITP = IRUN
        ENDIF
        IRET= ALGTDB(JUNIDB(0),LIST,-ITP)
      ENDIF
C
      KTC3X=IW(NTC3X)
      IF(KTC3X.EQ.0)THEN
        IER=4
        RETURN
      ENDIF
C
C++ Link to banks NDTK and NDDE
C
      INDNDE=IW(NAMIND('NDDE'))
      IF(INDNDE.EQ.0)THEN
        IER=2
        RETURN
      ENDIF
      KONDDE=INDNDE
      KCNDDE=IW(KONDDE+1)
      KRNDDE=IW(KONDDE+2)
C
      INDNTK=IW(NAMIND('NDTK'))
      IF(INDNTK.EQ.0)THEN
        IER=1
        RETURN
      ENDIF
      KONDTK=INDNTK
      KCNDTK=IW(KONDTK+1)
      KRNDTK=IW(KONDTK+2)
C   row number of track in NDTK bank
      IROWT=KONDTK+(KTN(ITK)-1)*KCNDTK+LMHLEN
C   Row number in NDDE bank
      I=IAND(ISHFT(IW(IROWT+JNDTTA),-19),MASK8)
      IF(I.EQ.0)THEN
        IER=3
        RETURN
      ENDIF
      IROWD=KONDDE+(I-1)*KCNDDE+LMHLEN
C
C++ Get the track momentum and error on momentum
C
      P=QP(ITK)
      SGP=FLOAT(IAND(ISHFT(IW(IROWT+JNDTTQ),-14),MASK9))/10000.0
      IF(SGP.GT.0.05)THEN
        NWAR = NWAR + 1
        MSG =
     &    '_NADDX_ WARNING: Track ??? has momentum error overflow (<5%)'
        WRITE(MSG(24:26),'(I3)') ITK
        IF(NWAR.LE.100) CALL QWMESE(MSG)
      ENDIF
C
C++ Get the particle's measured dE/dx and its relative error
C
      RI=FLOAT(IAND(ISHFT(IW(IROWD+JNDDLI),-12),MASK20))/100000.0
      RSIG=FLOAT(IAND(ISHFT(IW(IROWD+JNDDSE),-9),MASK23))/100000.0
      NS=IAND(IW(IROWD+JNDDSE),MASK9)
      TL=FLOAT(IAND(IW(IROWD+JNDDLI),MASK12))/10.0
C
C++ Measured dE/dx is already in RI
C++ TC3X correction is NOT applied in the nano production!
C++ So apply it here.
C
      RI=RI*RTABL(KTC3X,1,JTC3NR)
C
C++ Calculate expected dE/dx from R. Johnson's routine
C++ Sample length as determined in TIDHYP
C++ KITL(ITK) is track length (in mm!!), want SMPL sample length
C
      IF(NS.GT.0.AND.TL.GT.0.) THEN
        SMP=TL/FLOAT(NS)
        SMPL=ALOG(SMP/TWRSP)
      ELSE
        SMPL=0.
      ENDIF
C
C++ Loop over the N mass hypotheses
C
      IERR=0
      DO 300 I=1,N
        BG=P/RMASS(I)
        EBG=SGP/RMASS(I)
C
        CALL TXDEDX(BG,EBG,QH(I),RSIG,SMPL,RIEXP(I),SIGMA(I),IERR)
C
        IF(IERR.NE.0)THEN
          IER=IERR
          RETURN
        ENDIF
  300 CONTINUE
C
      IER=0
C
      RETURN
      END
#endif
