*DK ehdeps
      SUBROUTINE EHDEPS(NRJDP)
C.----------------------------------------------------------------
C       J.Badier - 11/12/85
C! Energy  e.m shower points
C       Tirage de l'energie deposee par les points d'une gerbe
C       selon la condition TIRAGE
C         Output : NRJDP Energie deposee en Kev
C  - Called by EHGERB
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ehpash.h"
#include "ehcond.h"
      EXTERNAL RNDM
C
         ALEA = RNDM(ALEA)
         IF(ALEA.LT.RHODEP) THEN
            ENRJ = ALEA * ENMAX1
         ELSE
            ENRJ = RNDM(NRJDP) * ENMAX2
         ENDIF
         NRJDP = NINT(ENRJ)
C
      RETURN
C
      END
#endif
