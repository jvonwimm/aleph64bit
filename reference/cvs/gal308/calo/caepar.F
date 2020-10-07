*DK caepar
      SUBROUTINE CAEPAR
C ----------------------------------------------------------------
C       J.Badier - 16/03/87
C! E.M shower coefficients
C ----- Calcul des coefficients de parametrisation d'une gerbe
C       electromagnetique.
C ----- NATGER = 1 : Electron.
C ----- NATGER = 2 : Photon.
C ----- NATGER = 3 : Pi 0
C   - Called by EHGERB
C ------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ehpash.h"
#include "capano.h"
      IF (EMGNRJ .LT. EMINLO) THEN
         EMALFA = 1.
         EMBETA = BETMAX
      ELSE
         ENLOG = ALOG( EMGNRJ )
         I = NATGER
         IF( EMGNRJ .LT. ETRANS .AND. NATGER .EQ. 1 ) I = 4
C ----- Shower parameters.
         ASURB = ASURB1(I) + ASURB2(I) * ENLOG
         UNSRB = UNSRB1(I) + UNSRB2(I) * ENLOG
C ----- Fluctuation.
         IF( EMGNRJ .GT. SEUSIG ) THEN
           SIGA = SQRT( SIGMA1(I) + SIGMA2(I) / EMGNRJ )
           SIGB = SQRT( SIGMB1(I) + SIGMB2(I) / EMGNRJ )
         ELSE
           SIGA = SIGMA3(I)
           SIGB = SIGMB3(I)
         ENDIF
         CALL RANNOR( ALEA1 , ALEA2 )
         SIGA = SIGA * ALEA1
         SIGB = SIGB * ALEA2
C ----- Calculate Alpha and Beta with some protections.
         UNSRA = ( 1. + SIGA ) * UNSRB / ASURB
         IF ( UNSRA .LE. USAMIN ) UNSRA = USAMIN
         IF ( UNSRA .GE. USAMAX ) UNSRA = USAMAX
         EMALFA = 1. / UNSRA
         BSURA = ( 1. + SIGB ) / ASURB
         IF ( BSURA .LE. BSUMIN ) BSURA = BSUMIN
         EMBETA = BSURA / UNSRA
         IF ( EMBETA .LT. BETMIN ) EMBETA = BETMIN
         IF ( EMBETA .GT. BETMAX ) EMBETA = BETMAX
      ENDIF
         EMALM1 = EMALFA - 1.
         IF( EMGNRJ .LE. EMINRA ) THEN
           METHCO = 1
         ELSE
           METHCO = 2
           PM2 = EXPRD0 + EXPRD1 * EMGNRJ
           EMAEXP =  1. / PM2
           EMFACT = PM2 / ( RFACT0 + RFACT1 * EMGNRJ )
           EMUDP0 = 5.
           EMUDP2 = .026
         ENDIF
      SMAXLR = -99.
      EMNAME = '    '
      RETURN
      END
#endif
