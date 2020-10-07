      SUBROUTINE EBIMPC( IFLG )
C ----------------------------------------------------
C   AUTHOR   : R.Clifft 08/06/88
C               J.Badier   29/11/89
C! Location of impact    -     crack or module.
CKEY PHOTONS CRACK IMPACT / INTERNAL
C
C
C     called by      EBRACK
C     calls          NONE
C
C     banks          NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
C            ETHC = energy threshold used in crack or module decision
C            ETH1 = RATIO1 threshold parameter
      PARAMETER ( ETHC = 0.5 , ETH1 = 0.5 , PTIT = .0001 )
      PARAMETER( TPC0 = .95 , TPC1 = .25 , TPC2 = .50 )
      PARAMETER( RPC0 = .93 , RPC1 = -.055 , RPC2 = .1 )
#include "ebenec.h"
C
C *** Condition flags to determine whether the impact was in a
C *** crack (IFLG = 2) or a module ( = 1)
C
      IFLG = 2
C
C *** Total energy is less than threshold
C
      IF(ENCRAT .LT. ETHC) GO TO 98
C
C *** RATIO1 is below threshold
C
      IF(RATIO1 .LT. ETH1) GO TO 1
C
      IF(R11STY .LT. PTIT) GO TO 98
C
C   R12STY is more than threshold
C
      IF(R12STY .GT. TPC0) GO TO 1
C
C *** RTHF is an empirical quantity,f(E,THETA),derived from M C studies
C *** which enables storey energy ratios to decide whether a photon
C *** was incident on a crack or a module.
C
      RTHF = RPC0 + RPC1 * ENCRAT + RPC2 * SINCEB
C
C Branch according to values of module and storey energy ratios
C
      IF( (R12STY .GT. RTHF .AND. RATIO2 .LT. TPC1) .OR.
     +    (R12STY .LT. RTHF .AND. R12STY .GT. TPC2  .AND.
     +     RATIO2 .LT. PTIT) )                     GO TO 1
C
      GO TO 98
C
    1 IFLG = 1
C
 98   CONTINUE
      RETURN
      END
#endif
