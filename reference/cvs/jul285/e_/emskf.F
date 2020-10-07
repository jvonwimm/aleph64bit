      SUBROUTINE EMSKF ( VIN,VSUM )
C=======================================================================
C!    Updates content of EMSK bank with new energy in mask
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C-
C-    Input : VIN  = E,THE,PHI from EMSK bank
C-            VSUM = new energy in mask (Ex,Ey,Ez components)
C-    Output: VIN  = updated E, THE, PHI
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
      DIMENSION VIN (3) , VSUM (3) , XV (3)
C
C-  Get energy already in road
C-  From theta & phi derive Ex,Ey,Ez components
C
      XV(3) = VIN(1)*COS(VIN(2))
      EN = SQRT (VSUM(1)**2+VSUM(2)**2+VSUM(3)**2)
      IF ( EN.GT.0.) THEN
      ET = VIN(1)*SIN(VIN(2))
      XV(1) = ET*COS(VIN(3))
      XV(2) = ET*SIN(VIN(3))
C
C-  Add vectorially to it new mask energy
C
      DO  135 IV=1,3
      XV(IV) = XV(IV)+VSUM(IV)
 135  CONTINUE
C
C-  Convert back to E, theta & phi
C
      VIN(1) = VIN (1) + EN
      ET = SQRT(XV(1)**2+XV(2)**2)
      VIN(2) = ATAN2(ET,XV(3))
      PHI    = ATAN2(XV(2),XV(1))
      IF(PHI.LT.0.) PHI =  PHI  + TWOPI
      IF(PHI.GT.TWOPI) PHI = PHI - TWOPI
      VIN(3) = PHI
      ENDIF
      RETURN
      END
#endif
