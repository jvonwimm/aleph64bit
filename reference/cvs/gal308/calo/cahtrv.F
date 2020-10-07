*DK cahtrv
      SUBROUTINE CAHTRV( ZLON , RADIS )
C.-----------------------------------------------------------------
C       J.Badier - 15/03/88
C!   Tirage hadronique transversal
C    Input : ZLON    Distance a l'origine de la gerbe en cm.
C    Output: RADIS   Distance a l'axe de la gerbe
C  - Called by CAHGRB
C
C.-----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "capano.h"
C
      EXTERNAL RNDM
C
C ----- Tirage radial
      KPASS = 0
C ----- Tirage de X1 , selon X1/(1+X1)**PUIS
C ----- La variable U1 permet un gain de temps.
   10 KPASS=KPASS+1
C ----- Tirage de U1 selon U1**(PUIS-1),HAPUIS = 1. / ( PUIS - 2 )
         U1 = RNDM(METH)**HAPUIS
C ----- Tirage de U1 selon (1-U1)*U1**(PUIS-1)
      IF( RNDM(U1) .GT. 1. - U1 ) GO TO 10
      X1 = 1. / U1 - 1.
C ----- Radis = r0 * x1
      RADIS = X1 * HADRAY * ZLON
      RETURN
      END
#endif
