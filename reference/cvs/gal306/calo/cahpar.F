*DK cahpar
      SUBROUTINE CAHPAR (ENER,IPART)
C ----------------------------------------------------------------
C       J.Badier - 16/03/87
C   Modified by G.Ganis        16/Jun/89
C! computes had. shower param.
C ----- Calcul des parametres de la gerbe hadronique.
C Input: ENER  Energie du track elem.
C        IPART  particle #
C ----- Voir HADCAL.
C ------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ehpash.h"
#include "capano.h"
C
      TOTNRJ = ENER
      ENLOG = ALOG( ENER )
      IF(IPART.EQ.LTYPEL.OR.IPART.EQ.LTYPGA) THEN
C ----- Electron geantino/ photon geantino
         EMGNRJ = ENER
         NATGER = 1
         HADNRJ = 0.
         IF (IPART .EQ. LTYPGA) NATGER = 2
      ELSE
C ----- Hadronic geantino.
C ----- Fraction e.m.
C ----- Je tire la queue.
         IF(TOTNRJ .GT. CNRJDA ) THEN
            CDAN = C1PRDA
         ELSE
            CDAN = C2PRDA + C3PRDA * TOTNRJ
         ENDIF
         ALEA = 2. * RNDM(ALEA) / CDAN
         XMIN = PIMADA / TOTNRJ
         SUIL = ( 1. - XMIN ) ** 2
         IF( ALEA .LT. SUIL ) THEN
            FRAC = 1. - SQRT( SUIL - ALEA )
         ELSE
C ----- Je tire la langue.
            IF( TOTNRJ .GT. ANRJDA ) THEN
               ADAN = A1PRDA + A2PRDA * TOTNRJ
            ELSE
               IF( TOTNRJ .GT. AMRJDA ) THEN
                  ADAN = A3PRDA
               ELSE
                  ADAN = A4PRDA + A5PRDA * TOTNRJ
               ENDIF
            ENDIF
            ADAN = ADAN - CDAN
            BADA = ( 2. - CDAN * SUIL ) / ADAN
            BADA = XMIN + SQRT(BADA)
            ALEA = RNDM(ALEA)
            ALEA = SQRT(ALEA)
            FRAC = BADA - ALEA * ( BADA - XMIN )
         ENDIF
         EMGNRJ = FRAC * TOTNRJ
C ----- Fraction hadronique.
         HADNRJ = ENER - EMGNRJ
C ----- Parametres longitudinaux.
         HALPHA = ALPHA0 + ALPHA1 * ENLOG
         HABETA = BETAH0 + BETAH1 * ENLOG
C ----- Parametres transversaux.
         HADRAY = RAYHA0 + RAYHA1 * ENER
         HAPUIS = 1. / ( PUISH0 - 2. )
         HADRAY = HAPUIS / HADRAY
C ----- Nature de la fraction e.m.
         NATGER = 5
      ENDIF
      SMAXLR = -99.
      SMAXLA = -99.
      EMNAME = '    '
      RETURN
      END
#endif
