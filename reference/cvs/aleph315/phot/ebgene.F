      SUBROUTINE EBGENE( NATU , ENER , ALPH , BETA , IER )
C-----------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C! Generation of the fluctuated parameters of an electron shower.
CKEY PHOTONS SHOWER PARAMETER / USER
C
C   Input :   NATU      Type of initial particle
C                   1 : Electron ( E > .1 Gev )
C                   2 : Photon.
C                   3 : Pi0.
C                   4 : Electron ( E < .1 Gev )
C                   5 : Pi0 from an interacting hadron.
C             ENER      Initial particle energy in Gev.
C
C   Output :  ALPH      Alpha shower parameter.
C             BETA      Beta shower parameter.
C             IER       Error, no generation.
C                       0 : OK
C                       1 : missing bank
C                       2 : energy out of range
C
C   BANKS :
C     INPUT   : EGPA    Parameters of a mean shower.
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Calls NAMIND
C ----------------------------------------------------
#ifndef DOC
      SAVE
C
C   Energy limits related to the EGPA bank.
      PARAMETER ( EMIN = .05 , EMAX = 50. , ETRA = .1 , SEUS = .5 )
      PARAMETER ( USAMI = .1 , USAMA = 2.5 ,BSAMI = .05 )
      PARAMETER ( BMIN = .1 , BMAX = 10. )
#include "egpajj.h"
#include "bcs.h"
      EXTERNAL NAMIND
      DATA NEGPA /0/
#include "bmacro.h"
C =========================================================
      IF (NEGPA .EQ. 0) NEGPA = NAMIND ('EGPA')
C
      IF ( ENER .LT. EMIN .OR. ENER .GT. EMAX ) GO TO 102
      KEGPA = IW(NEGPA)
      IF( KEGPA .EQ. 0 ) GO TO 101
      ENLG   = ALOG( ENER )
C
      KTYP = NATU
      IF ( ENER  .LT. ETRA ) KTYP = 4
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
     +           RTABL(KEGPA,KTYP,JEGPSI + 1) / ENER
        DBSA = RTABL(KEGPA,KTYP,JEGPSI + 3) +
     +           RTABL(KEGPA,KTYP,JEGPSI + 4) / ENER
      ELSE
        DUSA = RTABL(KEGPA,KTYP,JEGPSI + 2)
        DBSA = RTABL(KEGPA,KTYP,JEGPSI + 5)
      ENDIF
C
      CALL RANNOR( ALEA1 , ALEA2 )
C
      SUSA = SQRT( DUSA ) * ALEA1
      SBSA = SQRT( DBSA ) * ALEA2
C
      USRA = ( 1. + SUSA ) * USRA
      BSRA = ( 1. + SBSA ) * BSRA
      IF( USRA .LT. USAMI ) USRA = USAMI
      IF( USRA .GT. USAMA ) USRA = USAMA
      ALPH = 1. / USRA
      IF( BSRA .LT. BSAMI ) BSRA = BSAMI
      BETA = BSRA * ALPH
      IF( BETA .LT. BMIN ) BETA = BMIN
      IF( BETA .GT. BMAX ) BETA = BMAX
      IER = 0
      RETURN
C ===================== error ==========================
C   Missing bank.
  101 CONTINUE
      ALPH = 0.
      BETA = 0.
      IER = 1
      GO TO 98
C   Energy out of range
  102 CONTINUE
      ALPH = 0.
      BETA = 0.
      IER = 2
   98 CONTINUE
      END
#endif
