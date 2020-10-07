      FUNCTION FLUFAC(POL,AZI)
C----------------------------------------------------------------
C! ECAL resolution factor
C  Input  POL :  polar angle ( degrees )
C         AZI :  azimuthal angle ( degrees )
C
C  J.F. Grivaz  < 1989
C----------------------------------------------------------------
#include "resolu.h"
      DATA azimod /30./
C
C Resolution degradation
C
      flufac = 1.
C
C Cracks du barrel
C
      IF     ( pol .GT. polcor(2) ) THEN
        iaz = ABS(azi) / azimod
        raz = ABS(azi) - iaz * azimod
        IF ( raz .LT. azicra .OR. raz .GT. azimod-azicra )
     .  flufac = flufac * flucra
C
C Overlap Barrel/Endcap
C
      ELSEIF ( pol .GT. polcor(1) ) THEN
        flufac=flufac*flucor(1)
C
C Cracks du Endcap
C
      ELSEIF ( pol .GT. polcor(4) ) THEN
        iaz = ABS(azi+azimod/2.) / azimod
        raz = ABS(azi+azimod/2.) - iaz * azimod
        IF ( raz .LT. azicra .OR. raz .GT. azimod-azicra )
     .  flufac = flufac * flucra
C
C Crack Endcap/Lumi
C
      ELSEIF ( pol .GT. polcor(3) ) THEN
        flufac=flufac*flucor(2)
C
C Crack Lumi
C
      ELSE
        flufac = flufac * flulum
        raz = abs(azi) - 90.
        IF ( ABS(raz) .LT. azicrl ) flufac = flufac * flucrl
      ENDIF
C
  999 RETURN
      END
