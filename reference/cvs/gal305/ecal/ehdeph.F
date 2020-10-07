*DK ehdeph
      SUBROUTINE EHDEPH(NRJDP)
C --------------------------------------------------------------
C       J.Badier - 16/03/87
C! Energy for hadr. Shower
C       Energie deposee pour les points d'une gerbe hadronique.
C       L'unite est le kev.
C ----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ehpash.h"
C
C     BRETELLE PROVISOIRE VALABLE UNIQUEMENT POUR 10 GEV
C
      ALEA=RNDM(ALEA)*ECHMX
      IF(ALEA.LE.ECHDC)THEN
         ENRJ=ALEA*1000./EKEVH
      ELSE
         ENRJ=(ALEA-ECHDN)*1000.
      ENDIF
      NRJDP=NINT(ENRJ)
      RETURN
      END
#endif
