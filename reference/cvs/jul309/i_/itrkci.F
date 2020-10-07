      SUBROUTINE ITRKCI
C-----------------------------------------------------------------------
C! Fills commons with cuts used in ITC tracking.
C!
C!    Author      :- J. Sedgbeer 88/05/09
C!    Modified    :- I. Tomalin  89/06/02
C!    Modified    :- J. Sedgbeer 89/07/28
C!    Modified    :- J. Sedgbeer 90/01/06
C!
C!   Input:
C!      commons:     /ITWICC/  ITC geometry
C!                   /ITRKCC/  ITC resolution
C!      params:       ICUTJJ   ITC track finding cuts
C!
C!   Output:
C!      commons:     /ITRKER/   fill with default values
C!                   /ITRKPA/             "
C!
C!   called by : IINIRU
C!   calls     : INIGAP,AGETDB
C!
C!   Libraries required: ALEPHLIB
C!
C!======================================================================
#ifndef DOC
C I/O Commons etc.
#include "bcs.h"
#include "bosext.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "itwicc.h"
#include "iresol.h"
#include "itrker.h"
#include "itrkpa.h"
#include "icutjj.h"
C-----------------------------------------------------------------------
      LOGICAL FIRST
      EXTERNAL AGETDB
      INTEGER AGETDB
      REAL CTR0(3),SIG(3)
      DATA CTR0/50.0,25.0,12.5/
      DATA SIG/2.0,3.0,4.0/
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C----------------------------------------------------------------------
C Init. gap common
C
      CALL INIGAP
C
C Calculate error on difference in curvature between two links
C (assuming that they share a common coordinate).
      DO 40 I=3,8
        DO 30 J=2,I-1
          ER3 = SIGRIR(I)/(RWIRIT(I)*(RWIRIT(I)-RWIRIT(J)))
          DO 20 K=1,J-1
            ER2 = SIGRIR(J)/RWIRIT(J)*(RWIRIT(I)-RWIRIT(K))/
     +      ((RWIRIT(I)-RWIRIT(J))*(RWIRIT(J)-RWIRIT(K)))
            ER1 = SIGRIR(K)/(RWIRIT(K)*(RWIRIT(J)-RWIRIT(K)))
            CCRVIT(I,J,K) = 2.0*SQRT(ER1**2+ER2**2+ER3**2)
   20     CONTINUE
   30   CONTINUE
   40 CONTINUE
C
C Now define the cuts used by the ITC track finder. First check validity
C of ICUT bank.
C
      IRET = AGETDB('ICUT',IRUNRC)
      IF (FIRST.AND.IRET.GT.0.OR.IRET.LT.0) KICUT = IW(NAMIND('ICUT'))
      IF (IRET.EQ.0) CALL RERROR('ITRKCI',1,'Missing ITC DB banks.'//
     + ' Use default or previous values.')
C
C If first call and no bank for this run, then use default values.
C Otherwise fill from bank.
C   Fill /ITRKER/
C
      IF (FIRST.AND.IRET.EQ.0) THEN
C
        DO 10 I=1,3
C Set min. track radius to be searched for in ITC stand alone tracking.
          CTR0IT(I) = CTR0(I)
C Set no. of standard deviations error to be allowed in r-phi.
          SIGNUM(I) = SIG(I)
   10   CONTINUE
C
      ELSE IF (FIRST.AND.IRET.GT.0.OR.IRET.LT.0) THEN
C
        DO 15 I=1,3
          CTR0IT(I) = RW(KICUT+LMHLEN+JICURM-1+I)
          SIGNUM(I) = RW(KICUT+LMHLEN+JICUSR-1+I)
   15   CONTINUE
C
      END IF
C
C Fill /ITRKPA/  (tracking parameters)
C
      IF (FIRST.AND.IRET.EQ.0) THEN
C
C ********* Firstly do this for the TPC track extension phase. *********
C
C Minimum number of links allowed to make up a track.
        MNLINK(1) = 3
C Maximum number of layers jumped over by any link contributing to a
C track.
C  First specify what you would really like this to be.
        MXJMPD(1) = 2
C  Then specify what you have to choose because of limited CPU time
        MXJMPC(1) = 2
C Is z information to be used in tracking ?
        FZTSIT(1) = .FALSE.
C Number of standard deviations to be used with z resolution.
        ZFACIT(1) = 3.0
C Minimum confidence level of ITC track fit.
        CLMINI(1) = 0.010
C Minimum CHI**2 contribution of a coordinate to the total, for it
C to be considered for elimination from the ITC-TPC track.
        CHIELI = 4.0
C Minimum confidence level for increase in CHI**2 on adding ITC coords
C to TPC track.
        CLMNAD = 1.0E-4
C
C ********* Then do it for the ITC stand alone tracking phase. *********
C
C Minimum number of links allowed to make up a track.
        MNLINK(2) = 3
C Maximum number of layers jumped over by any link contributing to a
C track.
C  First specify what you would really like this to be.
        MXJMPD(2) = 2
C  Then specify what you have to choose because of limited CPU time
        MXJMPC(2) = 2
C Is z information to be used in tracking ?
        FZTSIT(2) = .FALSE.
C Number of standard deviations to be used with z resolution.
        ZFACIT(2) = 3.0
C Minimum confidence level of track fit.
        CLMINI(2) = 0.025
C
C ***** And matching of unextended TPC and ITC stand-alone tracks *****
C
C   Is matching to be used ?
        MATCHX = .TRUE.
C   Phis at scattering radius, RSCAIT, must differ by less than DELMIN.
        DELMIN = 0.15
C   CHI**2 of comparison of helix parameters must be less than CHI2PM.
        CHI2PM = 350.0
C   CHI**2 of final fit (IOPT=1) must be less than CHI2T1.
        CHI2T1 = 150.0
C   CHI**2 of final fit (IOPT=4) must be less than CHI2T4.
        CHI2T4 = 125.0
C
      ELSE IF (FIRST.AND.IRET.GT.0.OR.IRET.LT.0) THEN
C
        DO 45 J=1,2
          MNLINK(J) = IW(KICUT+LMHLEN+JICUML-1+J)
          MXJMPD(J) = IW(KICUT+LMHLEN+JICUJD-1+J)
          MXJMPC(J) = IW(KICUT+LMHLEN+JICUJC-1+J)
          FZTSIT(J) = IW(KICUT+LMHLEN+JICUUZ-1+J).EQ.1
          ZFACIT(J) = RW(KICUT+LMHLEN+JICUSZ-1+J)
          CLMINI(J) = RW(KICUT+LMHLEN+JICUCL-1+J)
   45   CONTINUE
        CHIELI = RW(KICUT+LMHLEN+JICUCP)
        CLMNAD = RW(KICUT+LMHLEN+JICUCA)
        MATCHX = IW(KICUT+LMHLEN+JICUMA).EQ.1
        DELMIN = RW(KICUT+LMHLEN+JICUDM)
        CHI2PM = RW(KICUT+LMHLEN+JICUCM)
        CHI2T1 = RW(KICUT+LMHLEN+JICUC1)
        CHI2T4 = RW(KICUT+LMHLEN+JICUC4)
C
      END IF
C
C Finally, try to make things fool proof !
      DO 50 I=1,2
        IF (MXJMPD(I).LT.0) FIRST = .FALSE.
        MXJMPD(I) = MAX(0,MXJMPD(I))
C
        IF (MXJMPC(I).LT.0) FIRST = .FALSE.
        MXJMPC(I) = MAX(0,MXJMPC(I))
C
        IF (MNLINK(I).LT.3) FIRST = .FALSE.
        MNLINK(I) = MAX(3,MNLINK(I))
C
        IF (MNLINK(I).GT.7) FIRST = .FALSE.
        MNLINK(I) = MIN(7,MNLINK(I))
C
        IF (MXJMPD(I).GT.JWIRIT-MNLINK(I)-1) FIRST = .FALSE.
        MXJMPD(I) = MIN(MXJMPD(I),JWIRIT-MNLINK(I)-1)
C
        IF (MXJMPC(I).GT.MXJMPD(I)) FIRST = .FALSE.
        MXJMPC(I) = MIN(MXJMPC(I),MXJMPD(I))
   50 CONTINUE
      IF (.NOT.FIRST) CALL RERROR('ITRKCI',2,
     +'The user has specified some silly ITC track selection cuts'
     +//'.MORE SENSIBLE ONES HAVE BEEN USED.')
C
      FIRST = .FALSE.
      END
#endif
