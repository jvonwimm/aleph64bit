      SUBROUTINE EBDEVI( NATU , ENER , ALPH , BETA , ECAR )
C-----------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C! Standard deviation of a shower from the mean distribution.
CKEY PHOTONS GAMMA DEVIATION / INTERNAL
C
C   Input :   NATU      Type of initial particle
C                   1 : Electron ( E > .1 Gev )
C                   2 : Photon.
C                   3 : Pi0.
C                   4 : Electron ( E < .1 Gev )
C                   5 : Pi0 from an interacting hadron.
C             ENER      Initial particle energy in Gev.
C             ALPH      Alpha shower parameter.
C             BETA      Beta shower parameter.
C
C   Output :  ECAR(1)   Standard deviation of 1. / Alpha.
C             ECAR(2)   Standard deviation of Beta / Alpha.
C                       Both set to 10. if uncalculable.
C
C   BANKS :
C     INPUT   : EGPA    Parameters of a mean shower.
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Called by EBPRGA
C ----------------------------------------------------
#ifndef DOC
      SAVE
C
      DIMENSION  ECAR(*)
C   Energy limits related to the EGPA bank.
      PARAMETER ( EMIN = .05 , ETRA = .1 , SEUS = .5 )
#include "egpajj.h"
#include "bcs.h"
      EXTERNAL NAMIND
      DATA ENDT / 0. /  , KTYP / 0 / , NEGPA/0/
#include "bmacro.h"
C =========================================================
      IF (NEGPA .EQ. 0) NEGPA = NAMIND ('EGPA')
      KEGPA = IW(NEGPA)
      IF (KEGPA .EQ. 0) GO TO 101
      IF (ENER .LT. EMIN .OR. ALPH .LE. 0.) GO TO 101
      IF (NATU .LE. 0 .OR. NATU .GT. 5) GO TO 101
C
      IF( ENER .NE. ENDT .OR. NATU .NE. KTYP ) THEN
C   First pass with this energy.
        ENDT = ENER
        ENLG = ALOG( ENDT )
C
        IF (ENDT  .LT. ETRA .AND. NATU .EQ. 1) THEN
C   Low energy electron.
          KTYP = 4
        ELSE
          KTYP = NATU
        ENDIF
C ----- Shower parameters.
        ASB  = RTABL(KEGPA,KTYP,JEGPAB) +
     +           ENLG * RTABL(KEGPA,KTYP,JEGPAB + 1)
        USB  = RTABL(KEGPA,KTYP,JEGPOB) +
     +           ENLG * RTABL(KEGPA,KTYP,JEGPOB + 1)
C ----- Calculate 1./Alpha and Beta/Alpha which are uncorrelated.
        USRA = USB / ASB
        BSRA = 1./ ASB
C ----- Estimation of the errors to the square.
        IF (ENER .GT. SEUS ) THEN
          DUSA = RTABL(KEGPA,KTYP,JEGPSI) +
     +     RTABL(KEGPA,KTYP,JEGPSI + 1) / ENDT
          DBSA = RTABL(KEGPA,KTYP,JEGPSI + 3) +
     +     RTABL(KEGPA,KTYP,JEGPSI + 4) / ENDT
        ELSE
          DUSA = RTABL(KEGPA,KTYP,JEGPSI + 2)
          DBSA = RTABL(KEGPA,KTYP,JEGPSI + 5)
        ENDIF
        SUSA = SQRT( DUSA )
        SBSA = SQRT( DBSA )
      ENDIF
C
C   Shower measured parameters.
      USAM = 1./ ALPH
      BSAM = BETA / ALPH
C   Deviations.
      ECAR(1) = ( USAM / USRA - 1. ) / SUSA
      ECAR(2) = ( BSAM / BSRA - 1. ) / SBSA
      RETURN
C ======================== error ===========================
C   Energy too small
  101 CONTINUE
      ECAR(1) = 10.
      ECAR(2) = 10.
      END
#endif
