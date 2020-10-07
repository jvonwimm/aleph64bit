      SUBROUTINE BTAGEV (NTRACK,NJET,JJET,JHEMI,PTRACK,NEGPROB,
     >                   PROBJET,PROBHEMI,PROBEVT)
CKEY   QIPBTAG / INTERNAL
C-----------------------------------------------------------------------
C! Probability for a jet to come from a b-quark
C  Called from QIPBTAG
C  SUBROUTINE BTAGEV
C  =================
C     PURPOSE :
C             calculate the probability of a jet to have the origine
C             in a B-Quark from the probabilities of the tracks
C
C     INPUT :
C             NTRACK               Number of good tracks
C             NJET                 Number of good jets
C             PTRACK(1:ntrack)     array with track-probabilities
C             JJET(1:ntrack)       Assignment of tracks to jets
C             JHEMI(1:ntrack)      Assignment of tracks to hemispheres
C             NEGPROB              Flag whether to use negative tracks
C
C     OUTPUT :
C             PROBJET(1:njet)      probability for each jet
C             PROBHEMI(1:2)        probability for hemispheres
C             PROBEVT              probability for event
C
C     CALLS   : VZERO
C                                      D. Brown,  M.Frank 13/1/92
C          revisions
C             D. BROWN 19-1-92       Add JJET,JHEMI,PWIND,PCUT  as arguments
C                      11-2-92       Protect factorials
C             M. FRANK 23-3-92       Protect exponentials
C             D. Brown 14-10-92      Protect agains underflows
C             D. Rousseau 7-5-93     Implement negative track probabilities
C             I. Tomalin  17-1-96    Protect against PROBEVT = 0 etc.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE       FIRST,LOGFAC
C  I/O variables
      INTEGER    NTRACK
      INTEGER    JJET(*),JHEMI(*)
      REAL       PROBEVT,PROBHEMI(2),PROBJET(*),PTRACK(*)
C--   local variables :
#include "maxtrk.h"
      INTEGER    MAXFAC
      PARAMETER ( MAXFAC = MAXTRK )
      INTEGER    IJET,ITRK,IHEMI,IFAC
      INTEGER    NTRK,NJET
      INTEGER    NTRK_HEMI(2)
      INTEGER    NTRK_JET(MAXJET)
      REAL*8     SUMEVT,SUMHEMI(2),SUMJET(MAXJET)
      REAL*8     LOGFAC(0:MAXFAC),LOGINVLOG,PROB,LOGPROB
      REAL*8     MINLOG
      LOGICAL    FIRST,NEGPROB,GOOD
      DATA       FIRST /.TRUE./,MINLOG/-50.0D0/
C-----------------------------------------------------------------------
C-- First time, setup factorial array.  We take the LOG of the factorials
C  to avoid overflow problems
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        LOGFAC(0) = 0D0
        DO IFAC=1,MAXFAC
          LOGFAC(IFAC) = LOGFAC(IFAC-1) + LOG(FLOAT(IFAC))
        END DO
      END IF
C
C  Initialize variables
C
      NTRK=0
      CALL VZERO(NTRK_JET,NJET)
      CALL VZERO(NTRK_HEMI,2)
      SUMEVT = 0D0
      DO IHEMI=1,2
        SUMHEMI(IHEMI) = 0D0
      END DO
      DO IJET=1,NJET
        SUMJET(IJET) = 0D0
      END DO
C
C  Now compute probability products
C
      DO ITRK=1,MIN(NTRACK,MAXTRK)
C
C  Compute the sum of the log of the probability for all tracks.
C
        GOOD = NEGPROB .OR. PTRACK(ITRK).GT.0E0
        IF(NEGPROB)THEN
          IF ( PTRACK(ITRK).GT.0E0 ) THEN
            LOGPROB          = LOG(PTRACK(ITRK)/2.)
          ELSE
            LOGPROB          = LOG(1.+PTRACK(ITRK)/2.)
          END IF
        ELSE
          IF ( PTRACK(ITRK).GT.0E0 ) THEN
            LOGPROB          = LOG(PTRACK(ITRK))
          END IF
        END IF
        IF(GOOD)THEN
          IJET             = JJET(ITRK)
          IHEMI            = JHEMI(ITRK)
          SUMEVT           = SUMEVT + LOGPROB
          SUMHEMI(IHEMI)   = SUMHEMI(IHEMI) + LOGPROB
          SUMJET(IJET)     = SUMJET(IJET) + LOGPROB
          NTRK             = NTRK + 1
          NTRK_HEMI(IHEMI) = NTRK_HEMI(IHEMI) + 1
          NTRK_JET(IJET)   = NTRK_JET(IJET) + 1
        END IF
      END DO
C
C  Normalize the probability product for the number of measurements made.
C  The normalization factor comes from interpreting the OVERALL probability
C  to be less than or equal to the observed value.  The trick here of looking
C  at the log of all the components and then taking the exponential avoids
C  floating overflows in cases where the number of tracks becomes very
C  large.
C
C  Events
C
      IF (SUMEVT.LT.0D0) THEN
        IF ( (NTRK-1) .GE. 1 ) THEN
            LOGINVLOG = LOG(-SUMEVT)
        END IF
        PROB    = 1D0
        DO ITRK = 1 , NTRK-1
          PROB  = PROB + EXP(ITRK*LOGINVLOG - LOGFAC(ITRK))
        END DO
        LOGPROB = LOG(PROB)
        PROBEVT = MIN(EXP(MAX(LOGPROB+SUMEVT,MINLOG)),1D0)
      ELSE
C Protection.
        PROBEVT = 1.0
      END IF
C
C  Hemispheres
C
      DO IHEMI=1,2
        IF (SUMHEMI(IHEMI).LT.0D0) THEN
          IF ( (NTRK_HEMI(IHEMI)-1) .GE. 1 ) THEN
              LOGINVLOG = LOG(-SUMHEMI(IHEMI))
          END IF
          PROB    = 1D0
          DO ITRK = 1,NTRK_HEMI(IHEMI) - 1
            PROB  = PROB + EXP(ITRK*LOGINVLOG - LOGFAC(ITRK))
          END DO
          LOGPROB = LOG(PROB)
          PROBHEMI(IHEMI) = MIN(EXP(MAX(LOGPROB+SUMHEMI(IHEMI),
     &         MINLOG)),1D0)
        ELSE
C Protection.
          PROBHEMI(IHEMI) = 1.0
        END IF
      END DO
C
C  Jets
C
      DO IJET=1,NJET
        IF (SUMJET(IJET).LT.0D0) THEN
          IF ( (NTRK_JET(IJET)-1) .GE. 1 ) THEN
              LOGINVLOG = LOG(-SUMJET(IJET))
          END IF
          PROB    = 1D0
          DO ITRK = 1,NTRK_JET(IJET)-1
            PROB  = PROB + EXP( ITRK*LOGINVLOG - LOGFAC(ITRK) )
          END DO
          LOGPROB = LOG(PROB)
          PROBJET(IJET) = MIN(EXP(MAX(LOGPROB+SUMJET(IJET),MINLOG)),1D0)
        ELSE
C Protection.
          PROBJET(IJET) = 1.0
        END IF
      END DO
C  done
      RETURN
      END
#endif
