*DK ehadrn
      SUBROUTINE EHADRN( ICOND , XGERB )
C.--------------------------------------------------------------
C        J.Badier - 17/03/87
C! Tirage d'une gerbe hadronique.
C   INPUT   ICOND = 0    Initialisation
C                   1    Tirage d'un point.
C   OUTPUT  XGERB        Coordonnees du point tire dans le systeme
C                        de la gerbe. XGERB(1) en cm selon l'axe.
C        Call RANNOR , RNDM.
C   Appele par EHGERBE
C. --------------------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION XGERB(3)
#include "capano.h"
      IF( ICOND .EQ. 0 ) THEN
         BABA = 0.
      ELSE
         XGERB(1) = 100. * RNDM(BABA)
         CALL RANNOR( ALEA2 , ALEA3 )
         XGERB(2) = XGERB(1) * HADRAY * ALEA2
         XGERB(3) = XGERB(1) * HADRAY * ALEA3
      ENDIF
      RETURN
      END
#endif
