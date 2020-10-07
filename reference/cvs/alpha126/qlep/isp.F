      SUBROUTINE ISP(MCT,MTRA,ISPLIT)
CKEY  QTRUTH/INTERNAL
C ...............................................................
C! Auxiliary to QTRUTH
C  Authors  D. Abbaneo , F. Ligabue 27/07/94  from FINLEP / UPHY
C  Modified for ISR and FSR 13/10/94
C  Called from QTRUTH
C .                                                             .
C . Input                                                       .
C .   MCT    / I   Alpha index of the true track                .
C .                                                             .
C . Output                                                      .
C .   MTRA   / I   Alpha index of the track mother which is     .
C .                produced by the string or by the last H.F.   .
C .                hadron (may be equal to MCT)                 .
C .                                                             .
C .   ISPLIT / I   End of the decay chain                       .
C .                 0   lepton from H.F. hadron                 .
C .                 1   K -> l                                  .
C .                 2   pi -> l                                 .
C .                 3   gamma -> l                              .
C .                 4   J/psi -> l                              .
C .                 5   psi' -> l                               .
C .                 6   pi0 -> l                                .
C .                 7   other -> l                              .
C .                 8   K                                       .
C .                 9   pi                                      .
C .                10   other                                   .
C .                                                             .
C ...............................................................
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
C
C - functions
      LOGICAL FHVFL,ENDCH
C
C - variables
      INTEGER MCT,MTRA,ISPLIT,MGM
C
C - macros
#include "qmacro.h"
C
C - anything
      ISPLIT=10
C
C - mother track
      MTRA=KMOTH(MCT,1)
      IF (MTRA.EQ.0) RETURN
      IF (FHVFL(MTRA).or.ENDCH(MTRA)) MTRA = MCT
C
C - pions and kaons
      IF (XPEQOR(MCT,'pi+')) THEN
        ISPLIT = 9
      ELSEIF (XPEQOR(MCT,'K+')) THEN
        ISPLIT = 8
C
C - objects decaying into leptons
      ELSEIF (XPEQOR(MCT,'e+').OR.XPEQOR(MCT,'mu+')
     @       .OR.XPEQOR(MCT,'tau+')) THEN
C
C - any decaying object
        ISPLIT=7
C
C - particular decaying objects
        IF (XPEQOR(MTRA,'K+'))    ISPLIT = 1
        IF (XPEQOR(MTRA,'pi+'))   ISPLIT = 2
        IF (XPEQOR(MTRA,'gamma')) ISPLIT = 3
        IF (XPEQOR(MTRA,'Jpsi'))  ISPLIT = 4
        IF (XPEQOR(MTRA,'psipr')) ISPLIT = 5
        IF (XPEQOR(MTRA,'pi0'))   ISPLIT = 6
C
C - if lepton coming from a H.F. hadron already set MTRA=MCT
        IF (MTRA.EQ.MCT) ISPLIT = 0
      ENDIF
C
c - recognize photons from ISR
      IF (isplit.eq.3) then
        if(KNMOTH(MTRA).EQ.0) THEN
          isplit = -3
          return
        elseif(xpeqor(kmoth(mtra,1),'Charged')) then
          isplit = -3
          return
        endif
      ENDIF
 79   MGM=KMOTH(MTRA,1)
C
c - recognize photons from FSR
      if(isplit.eq.3) then  
        IF ((KTPCOD(MGM).GE.100.AND.KTPCOD(MGM).LE.112).OR.
     .       (KTPCOD(MGM).EQ.44)) THEN
          ISPLIT = -3
          RETURN
        ENDIF
      endif
c
      IF (MGM.EQ.0) THEN
        MTRA=0
        RETURN
      ENDIF
C
C - go back to just after the string or the last H.F. hadron
      IF (FHVFL(MGM)) RETURN
      IF (ENDCH(MGM)) RETURN
      MTRA=MGM
      GOTO 79
      END
#endif
