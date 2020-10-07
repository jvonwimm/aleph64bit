*DK ehpalo
      FUNCTION EHPALO( X )
C.----------------------------------------------------------------
C       J.Badier - 11/12/85
C! Longitud. parametrisation
C       Forme reduite de la parametrisation longitudinale
C       EMALM1 = ALPHA - 1.
C       X = [BETA * (Z cm /Lrad cm)] / EMALM1
C    -Called by EHLOTI
C.------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "capano.h"
C
      EHPALO = X * EXP(1.-X)
      IF(EHPALO.LE.0.) EHPALO = 0.
      EHPALO = EHPALO ** EMALM1
C
      RETURN
      END
#endif
